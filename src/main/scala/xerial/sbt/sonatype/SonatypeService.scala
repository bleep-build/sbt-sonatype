package bleep
package plugin.sonatype.sonatype

import bleep.internal.FileUtils
import bleep.plugin.sonatype.sonatype.SonatypeClient.{StagingActivity, StagingProfile, StagingRepositoryProfile}
import bleep.plugin.sonatype.sonatype.SonatypeException.{MISSING_PROFILE, MISSING_STAGING_PROFILE, MULTIPLE_TARGETS, UNKNOWN_STAGE}
import ryddig.Logger
import wvlet.airframe.codec.MessageCodec

import java.io.File
import java.nio.file.Files
import scala.util.Try

/** Interface to access the REST API of Nexus
  * @param profileName
  */
class SonatypeService(
    logger: Logger,
    sonatypClient: SonatypeClient,
    val profileName: String,
    cacheToken: Option[String]
) extends AutoCloseable {
  import SonatypeService.*

  def this(logger: Logger, sonatypClient: SonatypeClient, profileName: String) = this(logger, sonatypClient, profileName, None)

  logger.info(s"sonatypeRepository  : ${sonatypClient.repoUri}")
  logger.info(s"sonatypeProfileName : ${profileName}")

  override def close(): Unit =
    sonatypClient.close()

  def findTargetRepository(command: CommandType, arg: Option[String]): StagingRepositoryProfile = {
    val repos = command match {
      case Close           => openRepositories
      case Promote         => closedRepositories
      case Drop            => stagingRepositoryProfiles()
      case CloseAndPromote => stagingRepositoryProfiles()
    }
    if (repos.isEmpty) {
      if (stagingProfiles.isEmpty) {
        logger.error(s"No staging profile found for $profileName")
        logger.error("Have you requested a staging profile and successfully published your signed artifact there?")
        throw SonatypeException(MISSING_STAGING_PROFILE, s"No staging profile found for $profileName")
      } else {
        throw new IllegalStateException(command.errNotFound)
      }
    }

    def findSpecifiedInArg(target: String) =
      repos.find(_.repositoryId == target).getOrElse {
        logger.error(s"Repository $target is not found")
        logger.error(s"Specify one of the repository ids in:\n${repos.mkString("\n")}")
        throw SonatypeException(UNKNOWN_STAGE, s"Repository $target is not found")
      }

    arg.map(findSpecifiedInArg).getOrElse {
      if (repos.size > 1) {
        logger.error(s"Multiple repositories are found:\n${repos.mkString("\n")}")
        logger.error(s"Specify one of the repository ids in the command line or run sonatypeDropAll to cleanup repositories")
        throw SonatypeException(MULTIPLE_TARGETS, "Found multiple staging repositories")
      } else {
        repos.head
      }
    }
  }

  def openRepositories = stagingRepositoryProfiles().filter(_.isOpen).sortBy(_.repositoryId)
  def closedRepositories = stagingRepositoryProfiles().filter(_.isClosed).sortBy(_.repositoryId)

  def uploadBundle(localBundlePath: File, deployPath: String): Unit =
    sonatypClient.uploadBundle(localBundlePath, deployPath)

  def openOrCreateByKey(descriptionKey: String): StagingRepositoryProfile = {
    // Find the already opened profile or create a new one
    val repos = findStagingRepositoryProfilesWithKey(descriptionKey)
    if (repos.size > 1) {
      throw SonatypeException(
        MULTIPLE_TARGETS,
        s"Multiple staging repositories for ${descriptionKey} exists. Run sonatypeDropAll first to clean up old repositories"
      )
    } else if (repos.size == 1) {
      val repo = repos.head
      logger.info(s"Found a staging repository ${repo}")
      repo
    } else {
      // Create a new staging repository by appending [sbt-sonatype] prefix to its description so that we can find the repository id later
      logger.info(s"No staging repository for ${descriptionKey} is found. Create a new one.")
      createStage(descriptionKey)
    }
  }

  def dropIfExistsByKey(descriptionKey: String): Option[StagingRepositoryProfile] = {
    // Drop the staging repository if exists
    val repos = findStagingRepositoryProfilesWithKey(descriptionKey)
    if (repos.isEmpty) {
      logger.info(s"No previous staging repository for ${descriptionKey} was found")
      None
    } else {
      repos.map { repo =>
        logger.info(s"Found a previous staging repository ${repo}")
        dropStage(repo)
      }.lastOption
    }
  }

  def findStagingRepositoryProfilesWithKey(descriptionKey: String): Seq[StagingRepositoryProfile] =
    stagingRepositoryProfiles(warnIfMissing = false).filter(_.description == descriptionKey)

  def stagingRepositoryProfiles(warnIfMissing: Boolean = true): Seq[StagingRepositoryProfile] = {
    // Note: using /staging/profile_repositories/(profile id) is preferred to reduce the response size,
    // but Sonatype API is quite slow (as of Sep 2019) so using a single request was much better.
    val response = sonatypClient.stagingRepositoryProfiles
    val myProfiles = response.filter(_.profileName == profileName)
    if (myProfiles.isEmpty && warnIfMissing) {
      logger.warn(s"No staging repository is found. Do publishSigned first.")
    }
    myProfiles
  }

  private def withCache[A](label: String, fileName: String, a: => A)(implicit codec: MessageCodec[A]): A = {
    val cachedir = (Vector("sbt", "sonatype") ++ cacheToken).mkString("-")
    val cacheRoot = new File(s"target/${cachedir}")
    val cacheFile = new File(cacheRoot, fileName)
    val value: A = if (cacheFile.exists() && cacheFile.length() > 0) {
      Try {
        val json = Files.readString(cacheFile.toPath)
        val retval = implicitly[MessageCodec[A]].fromJson(json)
        logger.info(s"Using cached ${label}...")
        retval
      }.getOrElse(a)
    } else {
      a
    }
    FileUtils.writeString(logger, None, cacheFile.toPath, implicitly[MessageCodec[A]].toJson(value))
    value
  }

  def stagingProfiles: Seq[StagingProfile] = {
    implicit val codec: MessageCodec[Seq[StagingProfile]] = MessageCodec.of
    val profiles = withCache("staging profiles", s"sonatype-profile-${profileName}.json", sonatypClient.stagingProfiles)
    profiles.filter(_.name == profileName)
  }

  lazy val currentProfile: StagingProfile = {
    val profiles = stagingProfiles
    if (profiles.isEmpty) {
      val error = MISSING_PROFILE(profileName, sonatypClient.repoUri.getHost)
      throw SonatypeException(error, error.message)
    }
    profiles.head
  }

  def createStage(description: String = "Requested by sbt-sonatype plugin"): StagingRepositoryProfile =
    sonatypClient.createStage(currentProfile, description)

  def closeStage(repo: StagingRepositoryProfile): StagingRepositoryProfile =
    if (repo.isClosed || repo.isReleased) {
      logger.info(s"Repository ${repo.repositoryId} is already closed")
      repo
    } else {
      sonatypClient.closeStage(currentProfile, repo)
    }

  def dropStage(repo: StagingRepositoryProfile): StagingRepositoryProfile = {
    sonatypClient.dropStage(currentProfile, repo).discard()
    logger.info(s"Dropped successfully: ${repo.repositoryId}")
    repo.toDropped
  }

  def promoteStage(repo: StagingRepositoryProfile): StagingRepositoryProfile = {
    if (repo.isReleased) {
      logger.info(s"Repository ${repo.repositoryId} is already released")
    } else {
      // Post promote(release) request
      sonatypClient.promoteStage(currentProfile, repo).discard()
    }
    dropStage(repo.toReleased)
  }

  def stagingRepositoryInfo(repositoryId: String) =
    sonatypClient.stagingRepository(repositoryId)

  def closeAndPromote(repo: StagingRepositoryProfile): StagingRepositoryProfile =
    if (repo.isReleased) {
      dropStage(repo)
    } else {
      val closed = closeStage(repo)
      promoteStage(closed)
    }

  def activities: Seq[(StagingRepositoryProfile, Seq[StagingActivity])] =
    for (r <- stagingRepositoryProfiles()) yield r -> sonatypClient.activitiesOf(r)
}

object SonatypeService {

  /** Switches of a Sonatype command to use
    */
  sealed trait CommandType {
    def errNotFound: String
  }
  case object Close extends CommandType {
    def errNotFound = "No open repository is found. Run publishSigned first"
  }
  case object Promote extends CommandType {
    def errNotFound = "No closed repository is found. Run publishSigned and close commands"
  }
  case object Drop extends CommandType {
    def errNotFound = "No staging repository is found. Run publishSigned first"
  }
  case object CloseAndPromote extends CommandType {
    def errNotFound = "No staging repository is found. Run publishSigned first"
  }
}
