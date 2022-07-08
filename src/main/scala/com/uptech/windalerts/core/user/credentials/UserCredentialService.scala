package com.uptech.windalerts.core.user.credentials

import cats.effect.Sync
import cats.implicits._
import cats.mtl.Raise
import cats.{Applicative, Monad}
import com.github.t3hnar.bcrypt._
import com.uptech.windalerts.core._
import com.uptech.windalerts.core.social.SocialPlatformType
import com.uptech.windalerts.core.types.{ChangePasswordRequest, RegisterRequest}


object UserCredentialService {
  def findByEmailAndPassword[F[_] : Sync]
  (
    email: String, password: String, deviceType: String
  )(implicit FR: Raise[F, UserAuthenticationFailedError],
    socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
    credentialsRepository: CredentialsRepository[F]): F[Credentials] =
    for {
      credentials <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).getOrElseF(FR.raise(UserAuthenticationFailedError(email)))
      passwordMatched <- isPasswordMatch(password, credentials)
    } yield passwordMatched

  private def isPasswordMatch[F[_] : Sync](password: String, creds: Credentials)(implicit FR: Raise[F, UserAuthenticationFailedError],
                                                                                 socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                                                                 credentialsRepository: CredentialsRepository[F]) = {
    if (password.isBcrypted(creds.password)) Applicative[F].pure(creds) else FR.raise(UserAuthenticationFailedError(creds.email))
  }

  def resetPassword[F[_] : Sync](
                                  email: String, deviceType: String
                                )(implicit UAF: Raise[F, UserAuthenticationFailedError],
                                  socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                  credentialsRepository: CredentialsRepository[F]): F[Credentials] =
    for {
      credentials <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).getOrElseF(UAF.raise(UserAuthenticationFailedError(email)))
      newPassword = utils.generateRandomString(10)
      _ <- credentialsRepository.updatePassword(credentials.id, newPassword.bcrypt)
    } yield credentials.copy(password = newPassword)

  def changePassword[F[_] : Sync](request: ChangePasswordRequest)(implicit UAR: Raise[F, UserAlreadyExistsRegistered], FR: Raise[F, UserAuthenticationFailedError],
                                                                  socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                                                  credentialsRepository: CredentialsRepository[F]): F[Unit] = {
    for {
      credentials <- findByEmailAndPassword(request.email, request.oldPassword, request.deviceType)
      result <- credentialsRepository.updatePassword(credentials.id, request.newPassword.bcrypt)
    } yield result
  }

  def register[F[_] : Sync](rr: RegisterRequest)
                           (implicit UAR: Raise[F, UserAlreadyExistsRegistered], FR: Raise[F, UserAuthenticationFailedError],
                            socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                            credentialsRepository: CredentialsRepository[F]): F[Credentials] = {
    for {
      _ <- notRegistered(rr.email, rr.deviceType)
      savedCredentials <- credentialsRepository.create(rr.email, rr.password.bcrypt, rr.deviceType)
    } yield savedCredentials
  }

  def notRegistered[F[_] : Sync](email: String, deviceType: String)(implicit FR: Raise[F, UserAlreadyExistsRegistered],
                                                                    socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                                                    credentialsRepository: CredentialsRepository[F]): F[Unit] = {
    for {
      notRegisteredAsEmailUser <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).isEmpty
      notRegisteredAsSocialUser <- notRegisteredAsSocialUser(email, deviceType)
      notRegistered <- if (notRegisteredAsEmailUser && notRegisteredAsSocialUser) {
        Applicative[F].pure(())
      } else {
        FR.raise(UserAlreadyExistsRegistered(email, deviceType))
      }
    } yield notRegistered
  }

  private def notRegisteredAsSocialUser[F[_] : Sync](email: String, deviceType: String)(implicit FR: Raise[F, UserAlreadyExistsRegistered], socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]]) = {
    socialCredentialsRepositories
      .values
      .map(_.find(email, deviceType).map(_.isDefined))
      .toList
      .sequence
      .map(!_.exists(_ == true))
  }
}

class UserCredentialService[F[_] : Sync](
                                          socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                          credentialsRepository: CredentialsRepository[F]) {
  def findByEmailAndPassword(
                              email: String, password: String, deviceType: String
                            )(implicit FR: Raise[F, UserAuthenticationFailedError]): F[Credentials] =
    for {
      credentials <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).getOrElseF(FR.raise(UserAuthenticationFailedError(email)))
      passwordMatched <- isPasswordMatch(password, credentials)
    } yield passwordMatched

  private def isPasswordMatch(password: String, creds: Credentials)(implicit FR: Raise[F, UserAuthenticationFailedError]) = {
    if (password.isBcrypted(creds.password)) Applicative[F].pure(creds) else FR.raise(UserAuthenticationFailedError(creds.email))
  }

  def resetPassword(
                     email: String, deviceType: String
                   )(implicit F: Monad[F], UAF: Raise[F, UserAuthenticationFailedError]): F[Credentials] =
    for {
      credentials <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).getOrElseF(UAF.raise(UserAuthenticationFailedError(email)))
      newPassword = utils.generateRandomString(10)
      _ <- credentialsRepository.updatePassword(credentials.id, newPassword.bcrypt)
    } yield credentials.copy(password = newPassword)

  def changePassword(request: ChangePasswordRequest)(implicit FR: Raise[F, UserAlreadyExistsRegistered], UAF: Raise[F, UserAuthenticationFailedError]): F[Unit] = {
    for {
      credentials <- findByEmailAndPassword(request.email, request.oldPassword, request.deviceType)
      result <- credentialsRepository.updatePassword(credentials.id, request.newPassword.bcrypt)
    } yield result
  }

  def register(rr: RegisterRequest)(implicit FR: Raise[F, UserAlreadyExistsRegistered]): F[Credentials] = {
    for {
      _ <- notRegistered(rr.email, rr.deviceType)
      savedCredentials <- credentialsRepository.create(rr.email, rr.password.bcrypt, rr.deviceType)
    } yield savedCredentials
  }

  def notRegistered(email: String, deviceType: String)(implicit FR: Raise[F, UserAlreadyExistsRegistered]): F[Unit] = {
    for {
      notRegisteredAsEmailUser <- credentialsRepository.findByEmailAndDeviceType(email, deviceType).isEmpty
      notRegisteredAsSocialUser <- notRegisteredAsSocialUser(email, deviceType)
      notRegistered <- if (notRegisteredAsEmailUser && notRegisteredAsSocialUser) {
        Applicative[F].pure(())
      } else {
        FR.raise(UserAlreadyExistsRegistered(email, deviceType))
      }
    } yield notRegistered
  }

  private def notRegisteredAsSocialUser(email: String, deviceType: String) = {
    socialCredentialsRepositories
      .values
      .map(_.find(email, deviceType).map(_.isDefined))
      .toList
      .sequence
      .map(!_.exists(_ == true))
  }
}
