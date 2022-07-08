package com.uptech.windalerts.core.user.sessions

import cats.Applicative
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.mtl.Raise
import com.uptech.windalerts.core._
import com.uptech.windalerts.core.user.credentials.UserCredentialService
import UserSessions._
import UserSessions.AccessTokenWithExpiry
import UserSessions.UserSession.REFRESH_TOKEN_EXPIRY
import com.uptech.windalerts.core.types.{AccessTokenRequest, LoginRequest}
import com.uptech.windalerts.core.user._
import dev.profunktor.auth.jwt.{JwtAuth, JwtSecretKey}
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}

import java.util.concurrent.TimeUnit

class UserSessions[F[_] : Sync](jwtKey: String,  userSessionsRepository: UserSessionRepository[F])(implicit A: Applicative[F]) {

  def reset(id: String, newDeviceToken: String)(implicit FR: Raise[F, UserNotFoundError]): F[Tokens] = {
    for {
      _ <- userSessionsRepository.deleteForUserId(id)
      tokens <- generateNewTokens(id, newDeviceToken)
    } yield tokens
  }

  def generateNewTokens(id: String, deviceToken: String): F[Tokens] = {
    val accessToken = createAccessToken(UserId(id))
    for {
      newRefreshToken <- userSessionsRepository.create(utils.generateRandomString(40), System.currentTimeMillis() + REFRESH_TOKEN_EXPIRY, id, deviceToken)
      tokens = Tokens(accessToken.accessToken, newRefreshToken, accessToken.expiredAt)
    } yield tokens
  }


  def createAccessToken(userId: UserId): AccessTokenWithExpiry = AccessTokenWithExpiry(JwtSecretKey(jwtKey), userId)

  def refresh(accessTokenRequest: AccessTokenRequest)(implicit FR: Raise[F, UserNotFoundError], RTNF: Raise[F, RefreshTokenNotFoundError], RTE: Raise[F, RefreshTokenExpiredError]): F[Tokens] = {
    for {
      oldRefreshToken <- userSessionsRepository.getByRefreshToken(accessTokenRequest.refreshToken)
      _ <- checkNotExpired(oldRefreshToken)
      _ <- userSessionsRepository.deleteForUserId(oldRefreshToken.userId)
      tokens <- generateNewTokens(oldRefreshToken.userId, oldRefreshToken.deviceToken)
    } yield tokens
  }

  private def checkNotExpired(oldRefreshToken: UserSession)(implicit RTNF: Raise[F, RefreshTokenExpiredError], A: Applicative[F]) =
    if (!oldRefreshToken.isExpired()) {
      A.pure(())
    } else {
      RTNF.raise(RefreshTokenExpiredError())
    }

  def updateDeviceToken(id: String, deviceToken: String)(implicit FR: Raise[F, UserNotFoundError]): F[Unit] = {
    userSessionsRepository.updateDeviceToken(id, deviceToken)
  }


  def deleteForUserId(userId: String): F[Unit] =
    userSessionsRepository.deleteForUserId(userId)


}

object UserSessions {

  case class AccessTokenWithExpiry(accessToken: String, expiredAt: Long)

  object AccessTokenWithExpiry {
    val ACCESS_TOKEN_EXPIRY = 6L * 60L * 60L * 1000L

    def apply(key: JwtSecretKey, userId: UserId): AccessTokenWithExpiry = {
      val current = System.currentTimeMillis()
      val expiry = current / 1000 + TimeUnit.MILLISECONDS.toSeconds(ACCESS_TOKEN_EXPIRY)
      val claims = JwtClaim(
        expiration = Some(expiry),
        issuedAt = Some(current / 1000),
        issuer = Some("wind-alerts.com"),
        subject = Some(userId.id)
      )

      AccessTokenWithExpiry(Jwt.encode(claims, key.value, JwtAlgorithm.HS256), expiry)
    }
  }

  case class UserSession(id: String, refreshToken: String, expiry: Long, userId: String, deviceToken: String) {
    def isExpired() = System.currentTimeMillis() > expiry
  }

  object UserSession {
    val REFRESH_TOKEN_EXPIRY = 14L * 24L * 60L * 60L * 1000L
  }




  def reset[F[_] : Sync](id: String, newDeviceToken: String)(implicit FR: Raise[F, UserNotFoundError],userSessionsRepository: UserSessionRepository[F]): F[Tokens] = {
    for {
      _ <- userSessionsRepository.deleteForUserId(id)
      tokens <- generateNewTokens(id, newDeviceToken)
    } yield tokens
  }

  def generateNewTokens[F[_] : Sync](id: String, deviceToken: String)(implicit userSessionsRepository: UserSessionRepository[F]): F[Tokens] = {
    val accessToken = createAccessToken(UserId(id))("")
    for {
      newRefreshToken <- userSessionsRepository.create(utils.generateRandomString(40), System.currentTimeMillis() + REFRESH_TOKEN_EXPIRY, id, deviceToken)
      tokens = Tokens(accessToken.accessToken, newRefreshToken, accessToken.expiredAt)
    } yield tokens
  }


  def createAccessToken[F[_] : Sync](userId: UserId)(jwtKey:String): AccessTokenWithExpiry = AccessTokenWithExpiry(JwtSecretKey(jwtKey), userId)

  def refresh[F[_] : Sync](accessTokenRequest: AccessTokenRequest)(implicit FR: Raise[F, UserNotFoundError], RTNF: Raise[F, RefreshTokenNotFoundError], RTE: Raise[F, RefreshTokenExpiredError], userSessionsRepository: UserSessionRepository[F]): F[Tokens] = {
    for {
      oldRefreshToken <- userSessionsRepository.getByRefreshToken(accessTokenRequest.refreshToken)
      _ <- checkNotExpired(oldRefreshToken)
      _ <- userSessionsRepository.deleteForUserId(oldRefreshToken.userId)
      tokens <- generateNewTokens(oldRefreshToken.userId, oldRefreshToken.deviceToken)
    } yield tokens
  }

  private def checkNotExpired[F[_] : Sync](oldRefreshToken: UserSession)(implicit RTNF: Raise[F, RefreshTokenExpiredError], A: Applicative[F],  userSessionsRepository: UserSessionRepository[F]) =
    if (!oldRefreshToken.isExpired()) {
      A.pure(())
    } else {
      RTNF.raise(RefreshTokenExpiredError())
    }

  def updateDeviceToken[F[_] : Sync](id: String, deviceToken: String)(implicit FR: Raise[F, UserNotFoundError], userSessionsRepository: UserSessionRepository[F]): F[Unit] = {
    userSessionsRepository.updateDeviceToken(id, deviceToken)
  }


  def deleteForUserId[F[_] : Sync](userId: String)(implicit userSessionsRepository: UserSessionRepository[F]): F[Unit] =
    userSessionsRepository.deleteForUserId(userId)

}