package bleep.plugin.sonatype.sonatype

import bleep.plugin.sonatype.sbt.sonatype.SonatypeCredentials
import bleep.plugin.sonatype.sonatype.SonatypeException.{BUNDLE_UPLOAD_FAILURE, JSON_PARSING_ERROR, STAGE_FAILURE, STATUS_CHECK_FAILURE, USER_ERROR}
import com.lumidion.sonatype.central.client.core.{CheckStatusResponse, DeploymentId, DeploymentName, DeploymentState, PublishingType}
import com.lumidion.sonatype.central.client.core.DeploymentState.PUBLISHED
import com.lumidion.sonatype.central.client.sttp.core.SyncSonatypeClient
import com.lumidion.sonatype.central.client.upickle.decoders.*
import bleep.plugin.sonatype.sonatype.utils.Extensions.*
import bleep.nosbt.librarymanagement.ivy.Credentials
import ryddig.Logger
import scala.math.pow
import scala.util.Try
import sttp.client4.{HttpError, ResponseException, SyncBackend}
import sttp.client4.httpurlconnection.HttpURLConnectionBackend
import sttp.client4.logging.slf4j.Slf4jLoggingBackend
import sttp.client4.logging.LoggingOptions
import sttp.client4.upicklejson.default.*
import sttp.model.StatusCode

import java.io.File

class SonatypeCentralClient(
    client: SyncSonatypeClient,
    backend: SyncBackend,
    readTimeoutMillis: Long,
    logger: Logger
) extends AutoCloseable {

  private def retryRequest[A, E](
      request: => Either[ResponseException[String, E], A],
      errorContext: String,
      errorCode: ErrorCode,
      retriesLeft: Int,
      retriesAttempted: Int = 0
  ): Either[SonatypeException, A] =
    for {
      response <- Try(request).toEither.leftMap { err =>
        SonatypeException(errorCode, s"$errorContext. ${err.getMessage}")
      }
      finalResponse <- response match {
        case Left(HttpError(message, code)) if (code == StatusCode.Forbidden) || (code == StatusCode.Unauthorized) || (code == StatusCode.BadRequest) =>
          Left(
            new SonatypeException(USER_ERROR, s"$errorContext. Status code: ${code.code}. Message Received: $message")
          )
        case Left(ex) =>
          if (retriesLeft > 0) {
            val exponent = pow(5, retriesAttempted).toInt
            val maximum = 30000
            val initialMillisecondsToSleep = 1500 + exponent
            val finalMillisecondsToSleep = if (maximum < initialMillisecondsToSleep) {
              maximum
            } else initialMillisecondsToSleep
            Thread.sleep(finalMillisecondsToSleep)
            logger.info(s"$errorContext. Request failed with the following message: ${ex.getMessage}. Retrying request.")
            retryRequest(request, errorContext, errorCode, retriesLeft - 1, retriesAttempted + 1)
          } else {
            Left(SonatypeException(errorCode, ex.getMessage))
          }
        case Right(res) => Right(res)
      }
    } yield finalResponse
  def uploadBundle(
      localBundlePath: File,
      deploymentName: DeploymentName,
      publishingType: Option[PublishingType]
  ): Either[SonatypeException, DeploymentId] = {
    logger.info(s"Uploading bundle ${localBundlePath.getPath} to Sonatype Central")

    retryRequest(
      client.uploadBundle(localBundlePath, deploymentName, publishingType).body,
      "Error uploading bundle to Sonatype Central",
      BUNDLE_UPLOAD_FAILURE,
      60
    )
  }

  /** Polls the deployment until it reaches a final state. A FAILED deployment is an error which carries what Central found wrong with each component: the
    * status response lists it under `errors`, which the client library's `CheckStatusResponse` does not decode, so the response is read as JSON here.
    */
  def awaitDeployment(
      deploymentId: DeploymentId,
      shouldDeployBePublished: Boolean
  ): Either[SonatypeException, Unit] =
    for {
      json <- retryRequest(
        client
          .checkStatusRequest(deploymentId, timeout = readTimeoutMillis)(asJson[CheckStatusResponse])
          .response(asJson[ujson.Value])
          .send(backend)
          .body,
        "Error checking deployment status",
        STATUS_CHECK_FAILURE,
        10
      )
      response <- Try(upickle.default.read[CheckStatusResponse](json)).toEither.leftMap { err =>
        SonatypeException(JSON_PARSING_ERROR, s"Could not read deployment status ${ujson.write(json)}. ${err.getMessage}")
      }
      finalRes <-
        if (response.deploymentState.isNonFinal) {
          Thread.sleep(5000L)
          awaitDeployment(deploymentId, shouldDeployBePublished)
        } else if (response.deploymentState == DeploymentState.FAILED) {
          Left(
            SonatypeException(
              STAGE_FAILURE,
              s"Deployment ${deploymentId.unapply} (${response.deploymentName.unapply}) failed. Central's errors:\n${SonatypeCentralClient.describeErrors(json)}"
            )
          )
        } else if (response.deploymentState != PUBLISHED && shouldDeployBePublished) {
          Thread.sleep(5000L)
          awaitDeployment(deploymentId, shouldDeployBePublished)
        } else {
          logger.info(
            s"Deployment succeeded for deployment id: ${deploymentId.unapply}. Current deployment state: ${response.deploymentState.unapply}"
          )
          Right(())
        }
    } yield finalRes

  override def close(): Unit = client.close()
}

object SonatypeCentralClient {
  val host: String = "central.sonatype.com"

  def fromCredentials(
      credentials: Seq[Credentials],
      readTimeoutMillis: Long,
      logger: Logger
  ): Either[SonatypeException, SonatypeCentralClient] =
    for {
      sonatypeCredentials <- SonatypeCredentials.fromEnv(credentials, host)
      backend = Slf4jLoggingBackend(HttpURLConnectionBackend())
      client = new SyncSonatypeClient(
        sonatypeCredentials.toSonatypeCentralCredentials,
        backend,
        Some(LoggingOptions(logRequestBody = Some(true), logResponseBody = Some(true)))
      )
    } yield new SonatypeCentralClient(client, backend, readTimeoutMillis, logger)

  /** Central's `errors`: for each component (a purl), the messages it failed validation with. */
  def describeErrors(status: ujson.Value): String =
    status.obj.get("errors") match {
      case None                         => "  (the status response has no errors field)"
      case Some(ujson.Obj(byComponent)) =>
        byComponent
          .map { case (component, messages) =>
            val lines = messages match {
              case ujson.Arr(ms) => ms.map(m => s"    - ${m match { case ujson.Str(str) => str; case other => ujson.write(other) }}")
              case other         => List(s"    - ${ujson.write(other)}")
            }
            (s"  $component" +: lines).mkString("\n")
          }
          .mkString("\n")
      case Some(other) => s"  ${ujson.write(other)}"
    }
}
