package com.uptech.windalerts.core.user

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import cats.mtl.Raise
import com.uptech.windalerts.core._
import com.uptech.windalerts.core.social.SocialPlatformType
import com.uptech.windalerts.core.user.credentials.{Credentials, CredentialsRepository, SocialCredentialsRepository, UserCredentialService}
import com.uptech.windalerts.core.types._
import com.uptech.windalerts.core.user.sessions.{UserSessionRepository, UserSessions}


class UserService[F[_] : Sync](userRepository: UserRepository[F],
                               userCredentialsService: UserCredentialService[F],
                               userSessions: UserSessions[F],
                               eventPublisher: EventPublisher[F],
                               passwordNotifier: PasswordNotifier[F]) {
  def register(registerRequest: RegisterRequest)(implicit FR: Raise[F, UserAlreadyExistsRegistered], M: Monad[F]): F[TokensWithUser] = {
    for {
      createUserResponse <- register_(registerRequest)
      tokens <- userSessions.generateNewTokens(createUserResponse._1.id, registerRequest.deviceToken)
      tokensWithUser = TokensWithUser(tokens, createUserResponse._1)
      _ <- eventPublisher.publishUserRegistered("userRegistered", UserRegistered(UserIdDTO(createUserResponse._1.id), EmailId(createUserResponse._1.email)))
    } yield tokensWithUser
  }

  def login(loginRequest: LoginRequest)(implicit FR: Raise[F, UserNotFoundError], UAF: Raise[F, UserAuthenticationFailedError]): F[TokensWithUser] = {
    for {
      persistedCredentials <- userCredentialsService.findByEmailAndPassword(loginRequest.email, loginRequest.password, loginRequest.deviceType)
      tokens <- userSessions.reset(persistedCredentials.id, loginRequest.deviceToken)
      tokensWithUser <- getTokensWithUser(persistedCredentials.id, tokens)
    } yield tokensWithUser
  }

  def refresh(accessTokenRequest: AccessTokenRequest)(implicit FR: Raise[F, UserNotFoundError], RTNF: Raise[F, RefreshTokenNotFoundError], RTE: Raise[F, RefreshTokenExpiredError]): F[TokensWithUser] = {
    for {
      tokens <- userSessions.refresh(accessTokenRequest)
      tokensWithUser <- getTokensWithUser(tokens.refreshToken.userId, tokens)
    } yield tokensWithUser
  }

  def getTokensWithUser(id: String, tokens: Tokens)(implicit FR: Raise[F, UserNotFoundError]): F[TokensWithUser] = {
    for {
      user <- getUser(id)
      tokensWithUser = TokensWithUser(tokens, user)
    } yield tokensWithUser
  }

  def resetPassword(
                     email: String, deviceType: String
                   )(implicit F: Monad[F], UAF: Raise[F, UserAuthenticationFailedError], UNF: Raise[F, UserNotFoundError]): F[Unit] =
    for {
      credentials <- userCredentialsService.resetPassword(email, deviceType)
      _ <- userSessions.deleteForUserId(credentials.id)
      user <- userRepository.getByUserId(credentials.id)
      _ <- passwordNotifier.notifyNewPassword(user.firstName(), email, credentials.password)
    } yield ()

  def logout(userId: String): F[Unit] =
    userSessions.deleteForUserId(userId)

  def register_(rr: RegisterRequest)(implicit FR: Raise[F, UserAlreadyExistsRegistered]): F[(UserT, Credentials)] = {
    for {
      savedCreds <- userCredentialsService.register(rr)
      saved <- userRepository.create(UserT.createEmailUser(savedCreds.id, rr.email, rr.name, rr.deviceType))
    } yield (saved, savedCreds)
  }

  def updateUserProfile(id: String, name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long)(implicit FR: Raise[F, UserNotFoundError]): F[UserT] = {
    for {
      user <- getUser(id)
      operationResult <- updateUser(name, snoozeTill, disableAllAlerts, notificationsPerHour, user)
    } yield operationResult
  }

  private def updateUser(name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long, user: UserT)(implicit FR: Raise[F, UserNotFoundError]) =
    userRepository.update(user.copy(name = name, snoozeTill = snoozeTill, disableAllAlerts = disableAllAlerts, notificationsPerHour = notificationsPerHour))

  def getUser(email: String, deviceType: String)(implicit FR: Raise[F, UserNotFoundError]): F[UserT] =
    userRepository.getByEmailAndDeviceType(email, deviceType)

  def getUser(userId: String)(implicit FR: Raise[F, UserNotFoundError]): F[UserT] =
    userRepository.getByUserId(userId)
}


object UserService {
  def register[F[_] : Sync](registerRequest: RegisterRequest)(implicit FR: Raise[F, UserAlreadyExistsRegistered], UAF:Raise[F, UserAuthenticationFailedError], M: Monad[F], userRepository: UserRepository[F], userSessionRepository: UserSessionRepository[F], socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]], credentialsRepository: CredentialsRepository[F], eventPublisher: EventPublisher[F]): F[TokensWithUser] = {
    for {
      createUserResponse <- register_(registerRequest)
      tokens <- UserSessions.generateNewTokens(createUserResponse._1.id, registerRequest.deviceToken)
      tokensWithUser = TokensWithUser(tokens, createUserResponse._1)
      _ <- eventPublisher.publishUserRegistered("userRegistered", UserRegistered(UserIdDTO(createUserResponse._1.id), EmailId(createUserResponse._1.email)))
    } yield tokensWithUser
  }

  def login[F[_] : Sync](loginRequest: LoginRequest)(implicit FR: Raise[F, UserNotFoundError], UAF: Raise[F, UserAuthenticationFailedError], userRepository: UserRepository[F], userSessionRepository: UserSessionRepository[F], socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]], credentialsRepository: CredentialsRepository[F]): F[TokensWithUser] = {
    for {
      persistedCredentials <- UserCredentialService.findByEmailAndPassword(loginRequest.email, loginRequest.password, loginRequest.deviceType)
      tokens <- UserSessions.reset(persistedCredentials.id, loginRequest.deviceToken)
      tokensWithUser <- getTokensWithUser(persistedCredentials.id, tokens)
    } yield tokensWithUser
  }

  def refresh[F[_] : Sync](accessTokenRequest: AccessTokenRequest)(implicit FR: Raise[F, UserNotFoundError], RTNF: Raise[F, RefreshTokenNotFoundError], RTE: Raise[F, RefreshTokenExpiredError], userSessionRepository: UserSessionRepository[F], userRepository: UserRepository[F]): F[TokensWithUser] = {
    for {
      tokens <- UserSessions.refresh(accessTokenRequest)
      tokensWithUser <- getTokensWithUser(tokens.refreshToken.userId, tokens)
    } yield tokensWithUser
  }

  def getTokensWithUser[F[_] : Sync](id: String, tokens: Tokens)(implicit FR: Raise[F, UserNotFoundError], userRepository: UserRepository[F]): F[TokensWithUser] = {
    for {
      user <- getUser(id)
      tokensWithUser = TokensWithUser(tokens, user)
    } yield tokensWithUser
  }

  def resetPassword[F[_] : Sync](
                                  email: String, deviceType: String
                                )(implicit F: Monad[F], UAF: Raise[F, UserAuthenticationFailedError], UNF: Raise[F, UserNotFoundError],
                                  userRepository: UserRepository[F],
                                  userSessionRepository: UserSessionRepository[F],
                                  socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                  credentialsRepository: CredentialsRepository[F],
                                  passwordNotifier: PasswordNotifier[F]): F[Unit] =
    for {
      credentials <- UserCredentialService.resetPassword(email, deviceType)
      _ <- UserSessions.deleteForUserId(credentials.id)
      user <- userRepository.getByUserId(credentials.id)
      _ <- passwordNotifier.notifyNewPassword(user.firstName(), email, credentials.password)
    } yield ()

  def logout[F[_] : Sync](userId: String)(implicit userSessionsRepository: UserSessionRepository[F]): F[Unit] =
    UserSessions.deleteForUserId(userId)

  def register_[F[_] : Sync](rr: RegisterRequest)(
    implicit FR: Raise[F, UserAlreadyExistsRegistered],
    UAF: Raise[F, UserAuthenticationFailedError],
    userRepository: UserRepository[F],
    userSessionRepository: UserSessionRepository[F],
    socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
    credentialsRepository: CredentialsRepository[F]): F[(UserT, Credentials)] = {
    for {
      savedCreds <- UserCredentialService.register(rr)
      saved <- userRepository.create(
        UserT.createEmailUser(savedCreds.id, rr.email, rr.name, rr.deviceType))
    } yield (saved, savedCreds)
  }

  def updateUserProfile[F[_] : Sync](id: String, name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long)(implicit FR: Raise[F, UserNotFoundError], userRepository: UserRepository[F]): F[UserT] = {
    for {
      user <- getUser(id)
      operationResult <- updateUser(name, snoozeTill, disableAllAlerts, notificationsPerHour, user)
    } yield operationResult
  }

  private def updateUser[F[_] : Sync](name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long, user: UserT)(implicit FR: Raise[F, UserNotFoundError], userRepository: UserRepository[F]) =
    userRepository.update(user.copy(name = name, snoozeTill = snoozeTill, disableAllAlerts = disableAllAlerts, notificationsPerHour = notificationsPerHour))

  def getUser[F[_] : Sync](email: String, deviceType: String)(implicit FR: Raise[F, UserNotFoundError], userRepository: UserRepository[F]): F[UserT] =
    userRepository.getByEmailAndDeviceType(email, deviceType)

  def getUser[F[_] : Sync](userId: String)(implicit FR: Raise[F, UserNotFoundError], userRepository: UserRepository[F]): F[UserT] =
    userRepository.getByUserId(userId)
}