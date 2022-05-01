//--------------------------------------
//
// Sonatype.scala
// Since: 2014/01/05
//
//--------------------------------------

package xerial.sbt

import bleep.logging.Logger
import bleep.model
import nosbt.librarymanagement.ivy.{Credentials, DirectCredentials}
import xerial.sbt.sonatype.SonatypeClient.StagingRepositoryProfile
import xerial.sbt.sonatype.SonatypeService._
import xerial.sbt.sonatype.{SonatypeClient, SonatypeException, SonatypeService}

import java.net.URI
import java.nio.file.Path
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.hashing.MurmurHash3

case class Sonatype(
    logger: Logger,
    sonatypeBundleDirectory: Path,
    /* Profile name at Sonatype: e.g. org.xerial */
    sonatypeProfileName: String,
    bundleName: String,
    version: String
)(implicit ec: ExecutionContext = ExecutionContext.global) {
  /* Sonatype repository URL: e.g. https://oss.sonatype.org/service/local */
  lazy val sonatypeRepository: String = s"https://$sonatypeCredentialHost/service/local"
  /* Credential host. Default is oss.sonatype.org */
  lazy val sonatypeCredentialHost: String = Sonatype.sonatypeLegacy

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
    else model.Repository.Folder(Some("sonatype-local-bundle"), sonatypeBundleDirectory)

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

    val directCredentials: DirectCredentials = {
      Credentials
        .forHost(credential.toList, sonatypeCredentialHost)
        .getOrElse {
          throw SonatypeException(
            SonatypeException.MISSING_CREDENTIAL,
            s"No credential is found for $sonatypeCredentialHost. Prepare ~/.sbt/(sbt_version)/sonatype.sbt file."
          )
        }
    }

    val sonatypeClient = new SonatypeClient(
      repositoryUrl = sonatypeRepository,
      directCredentials = directCredentials,
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
      rest.closeAndPromote(repo)
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
      rest.dropIfExistsByKey(descriptionKey)
      ()
    }

  /* Publish all staging repositories to Maven central */
  def sonatypeReleaseAll(): Unit =
    withSonatypeService { rest =>
      val tasks = rest.stagingRepositoryProfiles().map { repo =>
        Future(rest.closeAndPromote(repo))
      }
      val merged = Future.sequence(tasks)
      Await.result(merged, Duration.Inf)
      ()
    }

  /* Drop all staging repositories */
  def sonatypeDropAll(): Unit =
    withSonatypeService { rest =>
      val dropTasks = rest.stagingRepositoryProfiles().map { repo =>
        Future(rest.dropStage(repo))
      }
      val merged = Future.sequence(dropTasks)
      Await.result(merged, Duration.Inf)
      ()
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
  val github = "github.com"
  val gitlab = "gitlab.com"
}
