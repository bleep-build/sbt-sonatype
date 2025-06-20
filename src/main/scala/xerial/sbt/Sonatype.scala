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
import bleep.plugin.sonatype.sonatype.{SonatypeClient, SonatypeException, SonatypeService, SonatypeCentralClient, SonatypeCentralService}
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
    /* Credential host. Default is oss.sonatype.org */
    sonatypeCredentialHost: String = Sonatype.sonatypeLegacy
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

  /* Upload a bundle in sonatypeBundleDirectory and release it at Sonatype */
  def sonatypeBundleRelease(): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repo = prepare(rest)
      rest.uploadBundle(sonatypeBundleDirectory.toFile, repo.deployPath)
      rest.closeAndPromote(repo).discard()
      repo
    }

  /* Upload a bundle in sonatypeBundleDirectory */
  def sonatypeBundleUpload(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repo = sonatypeTargetRepositoryProfile.getOrElse {
        rest.openOrCreateByKey(sonatypeSessionName)
      }
      rest.uploadBundle(sonatypeBundleDirectory.toFile, repo.deployPath)
      repo
    }

  /* Clean (if exists) and create a staging repository for releasing the current version, then update publishTo */
  def sonatypePrepare(): StagingRepositoryProfile =
    withSonatypeService { rest =>
      prepare(rest)
    }

  /* Open (or create if not exists) to a staging repository for the current version, then update publishTo */
  def sonatypeOpen(): StagingRepositoryProfile =
    withSonatypeService { rest =>
      // Re-open or create a staging repository
      val repo = rest.openOrCreateByKey(sonatypeSessionName)
      repo
    }

  def sonatypeClose(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
      val repo1 = rest.findTargetRepository(Close, repoID)
      val repo2 = rest.closeStage(repo1)
      repo2
    }

  /* Promote a staging repository */
  def sonatypePromote(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
      val repo1 = rest.findTargetRepository(Promote, repoID)
      val repo2 = rest.promoteStage(repo1)
      repo2
    }

  /* Drop a staging repository */
  def sonatypeDrop(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
      val repo1 = rest.findTargetRepository(Drop, repoID)
      val repo2 = rest.dropStage(repo1)
      repo2
    }

  /* Publish with sonatypeClose and sonatypePromote */
  def sonatypeRelease(sonatypeTargetRepositoryProfile: Option[StagingRepositoryProfile]): StagingRepositoryProfile =
    withSonatypeService { rest =>
      val repoID = sonatypeTargetRepositoryProfile.map(_.repositoryId)
      val repo1 = rest.findTargetRepository(CloseAndPromote, repoID)
      val repo2 = rest.closeAndPromote(repo1)
      repo2
    }

  /* Clean a staging repository for the current version if it exists */
  def sonatypeClean(): Unit =
    withSonatypeService { rest =>
      val descriptionKey = sonatypeSessionName
      rest.dropIfExistsByKey(descriptionKey).discard()
    }

  /* Publish all staging repositories to Maven central */
  def sonatypeReleaseAll(): Unit =
    withSonatypeService { rest =>
      val tasks = rest.stagingRepositoryProfiles().map { repo =>
        Future(rest.closeAndPromote(repo))
      }
      val merged = Future.sequence(tasks)
      Await.result(merged, Duration.Inf).discard()
    }

  /* Drop all staging repositories */
  def sonatypeDropAll(): Unit =
    withSonatypeService { rest =>
      val dropTasks = rest.stagingRepositoryProfiles().map { repo =>
        Future(rest.dropStage(repo))
      }
      val merged = Future.sequence(dropTasks)
      Await.result(merged, Duration.Inf).discard()
    }

  /* Show staging activity logs at Sonatype */
  def sonatypeLog(): Unit =
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

  /* Show the list of staging repository profiles */
  def sonatypeStagingRepositoryProfiles(): Unit =
    withSonatypeService { rest =>
      val repos = rest.stagingRepositoryProfiles()
      if (repos.isEmpty)
        logger.warn(s"No staging repository is found for ${rest.profileName}")
      else {
        logger.info(s"Staging repository profiles (sonatypeProfileName:${rest.profileName}):")
        logger.info(repos.mkString("\n"))
      }
    }

  /* Show the list of staging profiles */
  def sonatypeStagingProfiles(): Unit =
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

/** Plugin for automating release processes at Sonatype Nexus
  */
object Sonatype {
  val sonatypeLegacy = "oss.sonatype.org"
  val sonatype01 = "s01.oss.sonatype.org"
  val sonatypeCentralHost = SonatypeCentralClient.host
  val knownOssHosts       = Seq(sonatypeLegacy, sonatype01)

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
  )(implicit ec: ExecutionContext): Unit = {
    if (sonatypeCredentialHost == sonatypeCentralHost) {
      // Use Central Portal API - simplified for now
      logger.info(s"Using Sonatype Central Portal for release")
      logger.warn("Central Portal API not fully implemented in bleep port yet")
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
}