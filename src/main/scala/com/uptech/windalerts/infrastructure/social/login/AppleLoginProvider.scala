package com.uptech.windalerts.infrastructure.social.login

import cats.Monad
import cats.effect.{Async, ContextShift}
import cats.implicits._
import com.softwaremill.sttp.{HttpURLConnectionBackend, sttp, _}
import com.turo.pushy.apns.auth.ApnsSigningKey
import com.uptech.windalerts.core.social.login.{SocialLoginProvider, SocialUser}
import com.uptech.windalerts.infrastructure.endpoints.codecs._
import com.uptech.windalerts.core.types.{AppleUser, TokenResponse}
import com.uptech.windalerts.infrastructure.social.login.AccessRequests.AppleRegisterRequest
import com.uptech.windalerts.logger
import io.circe.parser
import pdi.jwt._

import java.io.File
import java.security.PrivateKey
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AppleLoginProvider[F[_]](file: File)(implicit cs: ContextShift[F], s: Async[F], M: Monad[F]) extends SocialLoginProvider[F, AppleRegisterRequest] {
  val privateKey = getPrivateKey(file)
  override def fetchUserFromPlatform(credentials: AppleRegisterRequest): F[SocialUser] = {
    fetchUserFromPlatform_(credentials)
  }

  private def fetchUserFromPlatform_(credentials: AppleRegisterRequest) = {
    Async.fromFuture(M.pure(Future(getUser(credentials.authorizationCode))))
      .map(appleUser => SocialUser(appleUser.sub, appleUser.email, credentials.deviceType, credentials.deviceToken, credentials.name))
  }

  def getUser(authorizationCode: String): AppleUser = {
    getUser(authorizationCode, privateKey)
  }

  private def getUser(authorizationCode: String, privateKey: PrivateKey): AppleUser = {
    val req = sttp.body(Map(
      "client_id" -> "com.passiondigital.surfsup.ios",
      "client_secret" -> generateJWT(privateKey),
      "grant_type" -> "authorization_code",
      "code" -> authorizationCode,
    ))
      .post(uri"https://appleid.apple.com/auth/token?scope=email")

    implicit val backend = HttpURLConnectionBackend()

    val responseBody = req.send().body
    logger.info(s"Login response from apple ${responseBody.toString}")
    val tokenResponse = responseBody.flatMap(parser.parse(_)).flatMap(x => x.as[TokenResponse]).right.get
    val claims = Jwt.decode(tokenResponse.id_token, JwtOptions(signature = false))
    val parsedEither = parser.parse(claims.toOption.get.content)
    parsedEither.flatMap(x => x.as[AppleUser]).right.get
  }

  private def generateJWT(privateKey:PrivateKey) = {
    val current = System.currentTimeMillis()
    val claims = JwtClaim(
      issuer = Some("W9WH7WV85S"),
      audience = Some(Set("https://appleid.apple.com")),
      subject = Some("com.passiondigital.surfsup.ios"),
      expiration = Some(System.currentTimeMillis() / 1000 + (60 * 5)),
      issuedAt = Some(current / 1000)
    )
    val header = JwtHeader(JwtAlgorithm.ES256).withType(null).withKeyId("A423X8QGF3")
    Jwt.encode(header.toJson, claims.toJson, privateKey, JwtAlgorithm.ES256)
  }


  private def getPrivateKey(file: File) = {
    ApnsSigningKey.loadFromPkcs8File(file, "W9WH7WV85S", "A423X8QGF3")
  }

}