//--------------------------------------
//
// Sonatype.scala
// Since: 2014/01/05
//
//--------------------------------------

package bleep
package plugin.sonatype

import bleep.DiscardOps
import bleep.nosbt.librarymanagement.ivy.Credentials
import bleep.plugin.sonatype.sbt.sonatype.SonatypeCredentials
import bleep.plugin.sonatype.sonatype.SonatypeClient.StagingRepositoryProfile
import bleep.plugin.sonatype.sonatype.SonatypeService.*
import bleep.plugin.sonatype.sonatype.{SonatypeCentralClient, SonatypeCentralService, SonatypeClient, SonatypeException, SonatypeService}
import bleep.plugin.sonatype.sonatype.SonatypeException.GENERIC_ERROR
import com.lumidion.sonatype.central.client.core.{DeploymentName, PublishingType}
import ryddig.Logger

import java.net.URI
import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.hashing.MurmurHash3

/** Plugin for automating release processes at Sonatype Nexus
  */
case class Sonatype(
    logger: Logger,
    sonatypeBundleDirectory: Path,
    /* Profile name at Sonatype: e.g. org.xerial */
    sonatypeProfileName: String,
    bundleName: String,
    version: String,
    sonatypeCredentialHost: String = SonatypeCentralClient.host
)(implicit ec: ExecutionContext = ExecutionContext.global) {
  /* Sonatype repository URL: e.g. https://oss.sonatype.org/service/local */
  lazy val sonatypeRepository: String = s"https://$sonatypeCredentialHost/service/local"

  lazy val credential: Option[Credentials] =
    for {
      username <- sys.env.get("SONATYPE_USERNAME")
      password <- sys.env.get("SONATYPE_PASSWORD")
    } yield Credentials(
      "Sonatype Nexus Repository Manager",
      sonatypeCredentialHost,
      username,
      password
    )

  /* Default Sonatype publishTo target */
  lazy val sonatypePublishToBundle: model.Repository =
    // Sonatype snapshot repositories have no support for bundle upload,
    // so use direct publishing to the snapshot repo.
    if (version.endsWith("-SNAPSHOT")) sonatypeSnapshotResolver
    else model.Repository.MavenFolder(Some("sonatype-local-bundle"), sonatypeBundleDirectory)

  /* Sonatype snapshot resolver */
  lazy val sonatypeSnapshotResolver =
    model.Repository.Maven(
      Some(s"${sonatypeCredentialHost.replace('.', '-')}-snapshots"),
      new URI(s"https://$sonatypeCredentialHost/content/repositories/snapshots")
    )

  /* Sonatype staging resolver */
  lazy val sonatypeStagingResolver =
    model.Repository.Maven(
      Some(s"${sonatypeCredentialHost.replace('.', '-')}-staging"),
      new URI(s"https://$sonatypeCredentialHost/service/local/staging/deploy/maven2")
    )

  /* milliseconds before giving up Sonatype API requests */
  lazy val sonatypeTimeoutMillis = 60.toLong * 60 * 1000 // 60 minutes

  /* Used for identifying a sonatype staging repository */
  lazy val sonatypeSessionName = s"[sbt-sonatype] $bundleName $version"

  private def prepare(rest: SonatypeService): StagingRepositoryProfile = {
    logger.info(s"Preparing a new staging repository for $sonatypeSessionName")
    // Drop a previous staging repository if exists
    val dropTask = Future(rest.dropIfExistsByKey(sonatypeSessionName))
    // Create a new one
    val createTask = Future(rest.createStage(sonatypeSessionName))
    // Run two tasks in parallel
    val merged = dropTask.zip(createTask)
    val (droppedRepo @ _, createdRepo) = Await.result(merged, Duration.Inf)
    createdRepo
  }

  def withSonatypeService[T](body: SonatypeService => T): T = {
    val hashsum: String = {
      val input = Vector(sonatypeRepository, credential.toString, sonatypeCredentialHost).mkString("-")
      MurmurHash3.stringHash(input).abs.toString
    }

    val directCredentials: SonatypeCredentials =
      SonatypeCredentials.fromEnvOrError(credential.toList, sonatypeCredentialHost)

    val sonatypeClient = new SonatypeClient(
      repositoryUrl = sonatypeRepository,
      sonatypeCredentials = directCredentials,
      timeoutMillis = sonatypeTimeoutMillis,
      logger
    )
    val service = new SonatypeService(
      logger,
      sonatypeClient,
      sonatypeProfileName,
      Some(hashsum)
    )
    try body(service)
    finally service.close()
  }

  def withSonatypeCentralService[T](body: SonatypeCentralService => Either[SonatypeException, T]): Either[SonatypeException, T] = {
    val credentials = credential.toList
    
    for {
      client <- SonatypeCentralClient.fromCredentials(
        credentials,
        readTimeoutMillis = sonatypeTimeoutMillis.toLong,
        logger
      )
      service = new SonatypeCentralService(client, logger)
      res <- try {
        body(service)
      } catch {
        case e: Throwable => Left(new SonatypeException(GENERIC_ERROR, e.getMessage))
      } finally {
        client.close()
      }
    } yield res
  }

  /* Upload a bundle in sonatypeBundleDirectory and release it at Sonatype */
  def sonatypeBundleRelease(): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      // Use Central Portal for automatic release
      val deploymentName = s"$sonatypeProfileName.$bundleName-$version"
      val result = sonatypeCentralRelease(deploymentName)
      result match {
        case Left(ex) => throw ex
        case Right(_) => 
          // Return a dummy profile for Central Portal
          StagingRepositoryProfile(
            profileId = sonatypeProfileName,
            profileName = sonatypeProfileName,
            repositoryId = s"central-$version",
            `type` = "release",
            description = s"Central Portal deployment: $deploymentName"
          )
      }
    } else {
      // Use legacy OSSRH
      withSonatypeService { rest =>
        val repo = prepare(rest)
        rest.uploadBundle(sonatypeBundleDirectory.toFile, repo.deployPath)
        rest.closeAndPromote(repo).discard()
        repo
      }
    }

  /* Upload a bundle in sonatypeBundleDirectory */
  def sonatypeBundleUpload(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      // Use Central Portal for user-managed upload
      val deploymentName = s"$sonatypeProfileName.$bundleName-$version"
      val result = sonatypeCentralUpload(deploymentName)
      result match {
        case Left(ex) => throw ex
        case Right(_) => 
          // Return a dummy profile for Central Portal
          StagingRepositoryProfile(
            profileId = sonatypeProfileName,
            profileName = sonatypeProfileName,
            repositoryId = s"central-$version",
            `type` = "release",
            description = s"Central Portal deployment (user-managed): $deploymentName"
          )
      }
    } else {
      // Use legacy OSSRH
      withSonatypeService { rest =>
        val repo = sonatypeTargetRepositoryProfile.getOrElse {
          rest.openOrCreateByKey(sonatypeSessionName)
        }
        rest.uploadBundle(sonatypeBundleDirectory.toFile, repo.deployPath)
        repo
      }
    }

  /* Clean (if exists) and create a staging repository for releasing the current version, then update publishTo */
  def sonatypePrepare(): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        prepare(rest)
      }
    }

  /* Open (or create if not exists) to a staging repository for the current version, then update publishTo */
  def sonatypeOpen(): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        // Re-open or create a staging repository
        val repo = rest.openOrCreateByKey(sonatypeSessionName)
        repo
      }
    }

  def sonatypeClose(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
        val repo1 = rest.findTargetRepository(Close, repoID)
        val repo2 = rest.closeStage(repo1)
        repo2
      }
    }

  /* Promote a staging repository */
  def sonatypePromote(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
        val repo1 = rest.findTargetRepository(Promote, repoID)
        val repo2 = rest.promoteStage(repo1)
        repo2
      }
    }

  /* Drop a staging repository */
  def sonatypeDrop(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
        val repo1 = rest.findTargetRepository(Drop, repoID)
        val repo2 = rest.dropStage(repo1)
        repo2
      }
    }

  /* Publish with sonatypeClose and sonatypePromote */
  def sonatypeRelease(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      throw new UnsupportedOperationException("Staging repositories are not used with Sonatype Central Portal. Use sonatypeBundleRelease or sonatypeCentralRelease instead.")
    } else {
      withSonatypeService { rest =>
        val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
        val repo1 = rest.findTargetRepository(CloseAndPromote, repoID)
        val repo2 = rest.closeAndPromote(repo1)
        repo2
      }
    }

  /* Clean a staging repository for the current version if it exists */
  def sonatypeClean(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging repository cleanup is not applicable to Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val descriptionKey = sonatypeSessionName
        rest.dropIfExistsByKey(descriptionKey).discard()
      }
    }

  /* Publish all staging repositories to Maven central */
  def sonatypeReleaseAll(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val tasks = rest.stagingRepositoryProfiles().map { repo =>
          Future(rest.closeAndPromote(repo))
        }
        val merged = Future.sequence(tasks)
        Await.result(merged, Duration.Inf).discard()
      }
    }

  /* Drop all staging repositories */
  def sonatypeDropAll(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging repositories are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val dropTasks = rest.stagingRepositoryProfiles().map { repo =>
          Future(rest.dropStage(repo))
        }
        val merged = Future.sequence(dropTasks)
        Await.result(merged, Duration.Inf).discard()
      }
    }

  /* Show staging activity logs at Sonatype */
  def sonatypeLog(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging activity logs are not available with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val alist = rest.activities
        if (alist.isEmpty)
          logger.warn("No staging log is found")
        for ((repo, activities) <- alist) {
          logger.info(s"Staging activities of $repo:")
          for (a <- activities)
            a.showProgress(logger)
        }
        ()
      }
    }

  /* Show the list of staging repository profiles */
  def sonatypeStagingRepositoryProfiles(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging repository profiles are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val repos = rest.stagingRepositoryProfiles()
        if (repos.isEmpty)
          logger.warn(s"No staging repository is found for ${rest.profileName}")
        else {
          logger.info(s"Staging repository profiles (sonatypeProfileName:${rest.profileName}):")
          logger.info(repos.mkString("\n"))
        }
      }
    }

  /* Show the list of staging profiles */
  def sonatypeStagingProfiles(): Unit =
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      logger.info("Staging profiles are not used with Sonatype Central Portal")
    } else {
      withSonatypeService { rest =>
        val profiles = rest.stagingProfiles
        if (profiles.isEmpty)
          logger.warn(s"No staging profile is found for ${rest.profileName}")
        else {
          logger.info(s"Staging profiles (sonatypeProfileName:${rest.profileName}):")
          logger.info(profiles.mkString("\n"))
        }
      }
    }

  /* Upload bundle to Sonatype Central with user-managed publishing */
  def sonatypeCentralUpload(deploymentName: String = s"$sonatypeProfileName.$bundleName-$version"): Either[SonatypeException, Unit] = 
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      if (version.endsWith("-SNAPSHOT")) {
        Left(new SonatypeException(
          SonatypeException.USER_ERROR,
          "Version cannot be a snapshot version when deploying to sonatype central. Please ensure that the version is publishable and try again."
        ))
      } else {
        withSonatypeCentralService { service =>
          service.uploadBundle(
            sonatypeBundleDirectory.toFile,
            DeploymentName(deploymentName),
            PublishingType.USER_MANAGED
          )
        }
      }
    } else {
      Left(new SonatypeException(
        SonatypeException.USER_ERROR,
        s"sonatypeCredentialHost key needs to be set to ${SonatypeCentralClient.host} in order to release to sonatype central. Please adjust the key and try again."
      ))
    }

  /* Upload bundle to Sonatype Central with automatic publishing */
  def sonatypeCentralRelease(deploymentName: String = s"$sonatypeProfileName.$bundleName-$version"): Either[SonatypeException, Unit] = 
    if (sonatypeCredentialHost == SonatypeCentralClient.host) {
      if (version.endsWith("-SNAPSHOT")) {
        Left(new SonatypeException(
          SonatypeException.USER_ERROR,
          "Version cannot be a snapshot version when deploying to sonatype central. Please ensure that the version is publishable and try again."
        ))
      } else {
        withSonatypeCentralService { service =>
          service.uploadBundle(
            sonatypeBundleDirectory.toFile,
            DeploymentName(deploymentName),
            PublishingType.AUTOMATIC
          )
        }
      }
    } else {
      Left(new SonatypeException(
        SonatypeException.USER_ERROR,
        s"sonatypeCredentialHost key needs to be set to ${SonatypeCentralClient.host} in order to release to sonatype central. Please adjust the key and try again."
      ))
    }
}

/** Plugin for automating release processes at Sonatype Nexus
  */
object Sonatype {
  val sonatypeLegacy = "oss.sonatype.org"
  val sonatype01 = "s01.oss.sonatype.org"
  val sonatypeCentralHost = SonatypeCentralClient.host
  val knownOssHosts = Seq(sonatypeLegacy, sonatype01)

  val github = "github.com"
  val gitlab = "gitlab.com"

  /** Automatic bundle release with routing based on credential host */
  def bundleRelease(
      logger: Logger,
      sonatypeBundleDirectory: Path,
      sonatypeProfileName: String,
      bundleName: String,
      version: String,
      sonatypeCredentialHost: String = sonatypeLegacy
  )(implicit ec: ExecutionContext): Unit =
    if (sonatypeCredentialHost == sonatypeCentralHost) {
      // Use Central Portal API
      logger.info(s"Using Sonatype Central Portal for release")
      
      if (version.endsWith("-SNAPSHOT")) {
        logger.error("Version cannot be a snapshot version when deploying to Sonatype Central. Please ensure that the version is publishable and try again.")
        throw new Exception("Cannot deploy SNAPSHOT versions to Sonatype Central")
      }
      
      val sonatype = Sonatype(
        logger,
        sonatypeBundleDirectory,
        sonatypeProfileName,
        bundleName,
        version,
        sonatypeCredentialHost
      )
      
      val deploymentName = DeploymentName(s"${sonatypeProfileName}.${bundleName}-${version}")
      
      val result = sonatype.withSonatypeCentralService { service =>
        service.uploadBundle(
          sonatypeBundleDirectory.toFile,
          deploymentName,
          PublishingType.AUTOMATIC
        )
      }
      
      result match {
        case Left(ex) => throw ex
        case Right(_) => logger.info("Successfully uploaded bundle to Sonatype Central for automatic release")
      }
    } else {
      // Use legacy OSSRH API
      logger.info(s"Using legacy Sonatype OSSRH for release")
      val sonatype = Sonatype(
        logger,
        sonatypeBundleDirectory,
        sonatypeProfileName,
        bundleName,
        version,
        sonatypeCredentialHost
      )
      sonatype.sonatypeBundleRelease().discard()
    }
}
