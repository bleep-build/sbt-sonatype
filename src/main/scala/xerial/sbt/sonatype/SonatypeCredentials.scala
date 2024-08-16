package bleep.plugin.sonatype.sbt.sonatype

import bleep.nosbt.librarymanagement.ivy.Credentials
import bleep.plugin.sonatype.sonatype.SonatypeException
import bleep.plugin.sonatype.sonatype.SonatypeException.MISSING_CREDENTIAL
import bleep.plugin.sonatype.sonatype.utils.Extensions.*
import com.lumidion.sonatype.central.client.core.SonatypeCredentials as SonatypeCentralCredentials

import java.nio.charset.StandardCharsets
import java.util.Base64

final case class SonatypeCredentials private (userName: String, password: String) {
  override def toString: String = "SonatypeCredentials(userName: <redacted>, password: <redacted>)"

  def toBase64: String = Base64.getEncoder.encodeToString(s"${userName}:${password}".getBytes(StandardCharsets.UTF_8))

  def toSonatypeCentralCredentials: SonatypeCentralCredentials = SonatypeCentralCredentials(userName, password)
}

object SonatypeCredentials {
  def fromEnv(
      credentials: Seq[Credentials],
      credentialHost: String
  ): Either[SonatypeException, SonatypeCredentials] = {
    Credentials
      .forHost(credentials, credentialHost)
      .toRight {
        SonatypeException(
          MISSING_CREDENTIAL,
          s"No credential is found for ${credentialHost}. Prepare ~/.sbt/(sbt_version)/sonatype.sbt file."
        )
      }
      .map(directCredentials => SonatypeCredentials(directCredentials.userName, directCredentials.passwd))
  }

  def fromEnvOrError(credentials: Seq[Credentials], credentialHost: String): SonatypeCredentials =
    fromEnv(credentials, credentialHost).getOrError
}
