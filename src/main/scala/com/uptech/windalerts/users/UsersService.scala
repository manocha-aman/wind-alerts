package com.uptech.windalerts.users

import cats.data.{EitherT, OptionT}
import cats.effect.Sync
import com.github.t3hnar.bcrypt._
import com.restfb.{DefaultFacebookClient, Parameter, Version}
import com.softwaremill.sttp.{HttpURLConnectionBackend, sttp, _}
import com.uptech.windalerts.Repos
import com.uptech.windalerts.domain.codecs._
import com.uptech.windalerts.domain.domain.UserType._
import com.uptech.windalerts.domain.domain._
import com.uptech.windalerts.domain.{conversions, domain, secrets}
import io.circe.optics.JsonPath.root
import io.circe.parser
import io.circe.syntax._
import io.scalaland.chimney.dsl._
import org.mongodb.scala.bson.ObjectId

class UserService[F[_] : Sync](rpos: Repos[F]) {

  def verifyEmail(id: String) = {
    def makeUserTrial(user: UserT): EitherT[F, ValidationError, UserT] = {
      rpos.usersRepo().update(user.copy(
        userType = Trial.value,
        startTrialAt = System.currentTimeMillis(),
        endTrialAt = System.currentTimeMillis() + (30L * 24L * 60L * 60L * 1000L),
      )).toRight(CouldNotUpdateUserError())
    }

    def startTrial(id: String): EitherT[F, ValidationError, UserT] = {
      for {
        user <- getUser(id)
        operationResult <- makeUserTrial(user)
      } yield operationResult
    }

    for {
      operationResult <- startTrial(id)
    } yield operationResult
  }


  def updateSubscribedUserRole(user: UserId, startTime: Long, expiryTime: Long) = {
    def withTypeFixed(start: Long, expiry: Long, user: UserT): EitherT[F, ValidationError, UserT] = {
      rpos.usersRepo().update(user.copy(userType = Premium.value, lastPaymentAt = start, nextPaymentAt = expiry)).toRight(CouldNotUpdateUserError())
    }

    def makeUserPremium(id: String, start: Long, expiry: Long): EitherT[F, ValidationError, UserT] = {
      for {
        user <- getUser(id)
        operationResult <- withTypeFixed(start, expiry, user)
      } yield operationResult
    }

    def makeUserPremiumExpired(userId: String): EitherT[F, ValidationError, UserT] = {
      for {
        user <- getUser(userId)
        operationResult <- rpos.usersRepo().update(user.copy(userType = PremiumExpired.value, nextPaymentAt = -1)).toRight(CouldNotUpdateUserError())
        _ <- EitherT.liftF(rpos.alertsRepository().disableAllButOneAlerts(userId))
      } yield operationResult
    }

    if (expiryTime > System.currentTimeMillis()) {
      makeUserPremium(user.id, startTime, expiryTime)
    } else {
      makeUserPremiumExpired(user.id)
    }
  }

  def updateUserProfile(id: String, name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long): EitherT[F, ValidationError, UserT] = {
    for {
      user <- getUser(id)
      operationResult <- updateUser(name, snoozeTill, disableAllAlerts, notificationsPerHour, user)
    } yield operationResult
  }

  private def updateUser(name: String, snoozeTill: Long, disableAllAlerts: Boolean, notificationsPerHour: Long, user: UserT): EitherT[F, ValidationError, UserT] = {
    rpos.usersRepo().update(user.copy(name = name, snoozeTill = snoozeTill, disableAllAlerts = disableAllAlerts, notificationsPerHour = notificationsPerHour, lastPaymentAt = -1, nextPaymentAt = -1)).toRight(CouldNotUpdateUserError())
  }

  def updateDeviceToken(userId: String, deviceToken: String) =
    rpos.usersRepo().updateDeviceToken(userId, deviceToken)

  def updatePassword(userId: String, password: String): OptionT[F, Unit] =
    rpos.credentialsRepo().updatePassword(userId, password.bcrypt)


  def getFacebookUserByAccessToken(accessToken: String, deviceType: String) = {
    for {
      facebookClient <- EitherT.pure((new DefaultFacebookClient(accessToken, rpos.fbSecret(), Version.LATEST)))
      facebookUser <- EitherT.pure((facebookClient.fetchObject("me", classOf[com.restfb.types.User], Parameter.`with`("fields", "name,id,email"))))
      dbUser <- getUser(facebookUser.getEmail, deviceType)
    } yield dbUser
  }

  def createUser(rr: FacebookRegisterRequest): EitherT[F, UserAlreadyExistsError, (UserT, FacebookCredentialsT)] = {
    for {
      facebookClient <- EitherT.pure(new DefaultFacebookClient(rr.accessToken, rpos.fbSecret(), Version.LATEST))
      facebookUser <- EitherT.pure((facebookClient.fetchObject("me", classOf[com.restfb.types.User], Parameter.`with`("fields", "name,id,email"))))
      _ <- doesNotExist(facebookUser.getEmail, rr.deviceType)

      savedCreds <- EitherT.liftF(rpos.facebookCredentialsRepo().create(FacebookCredentialsT(facebookUser.getEmail, rr.accessToken, rr.deviceType)))
      savedUser <- EitherT.liftF(rpos.usersRepo().create(UserT.create(new ObjectId(savedCreds._id.toHexString), facebookUser.getEmail, facebookUser.getName, rr.deviceId, rr.deviceToken, rr.deviceType, System.currentTimeMillis(), Trial.value, -1, false, 4)))
    } yield (savedUser, savedCreds)
  }

  def createUser(rr: AppleRegisterRequest): EitherT[F, UserAlreadyExistsError, (UserT, AppleCredentials)] = {
    for {
      appleUser <- EitherT.pure(AppleLogin.getUser(rr.authorizationCode, rpos.appleLoginConf()))
      _ <- doesNotExist(appleUser.email, rr.deviceType)

      savedCreds <- EitherT.liftF(rpos.appleCredentialsRepository().create(AppleCredentials(appleUser.email, rr.deviceType, appleUser.sub)))
      savedUser <- EitherT.liftF(rpos.usersRepo().create(UserT.create(new ObjectId(savedCreds._id.toHexString), appleUser.email, rr.name, rr.deviceId, rr.deviceToken, rr.deviceType, System.currentTimeMillis(), Trial.value, -1, false, 4)))
    } yield (savedUser, savedCreds)
  }

  def loginUser(rr: AppleLoginRequest) = {
    for {
      appleUser <- EitherT.pure(AppleLogin.getUser(rr.authorizationCode, rpos.appleLoginConf()))
      _ <- rpos.appleCredentialsRepository().findByAppleId(appleUser.sub)
      dbUser <- getUser(appleUser.email, rr.deviceType)
    } yield dbUser
  }

  def createUser(rr: RegisterRequest): EitherT[F, UserAlreadyExistsError, UserT] = {
    val credentials = Credentials(rr.email, rr.password.bcrypt, rr.deviceType)
    for {
      _ <- doesNotExist(credentials.email, credentials.deviceType)
      savedCreds <- EitherT.liftF(rpos.credentialsRepo().create(credentials))
      saved <- EitherT.liftF(rpos.usersRepo().create(UserT.create(new ObjectId(savedCreds._id.toHexString), rr.email, rr.name, rr.deviceId, rr.deviceToken, rr.deviceType, -1, Registered.value, -1, false, 4)))
    } yield saved
  }

  def logoutUser(userId: String) = {
    for {
      _ <- EitherT.liftF(rpos.refreshTokenRepo().deleteForUserId(userId))
      _ <- updateDeviceToken(userId, "").toRight(CouldNotUpdateUserError())
    } yield ()
  }

  def doesNotExist(email: String, deviceType: String) = {
    for {
      emailDoesNotExist <- countToEither(rpos.credentialsRepo().count(email, deviceType))
      facebookDoesNotExist <- countToEither(rpos.facebookCredentialsRepo().count(email, deviceType))
      appleDoesNotExist <- countToEither(rpos.appleCredentialsRepository().count(email, deviceType))
    } yield (emailDoesNotExist, facebookDoesNotExist, appleDoesNotExist)
  }

  private def countToEither(count: F[Int]) = {
    val fbccdentialDoesNotExist: EitherT[F, UserAlreadyExistsError, Unit] = EitherT.liftF(count).flatMap(c => {
      val e: Either[UserAlreadyExistsError, Unit] = if (c > 0) Left(UserAlreadyExistsError("", ""))
      else Right(())
      EitherT.fromEither(e)
    })
    fbccdentialDoesNotExist
  }


  private def toEither(user: UserT): Either[UserNotFoundError, UserT] = {
    Right(user)
  }

  def getUser(email: String, deviceType: String): EitherT[F, UserNotFoundError, UserT] =
    OptionT(rpos.usersRepo().getByEmailAndDeviceType(email, deviceType)).toRight(UserNotFoundError())

  def getUser(userId: String): EitherT[F, ValidationError, UserT] =
    OptionT(rpos.usersRepo().getByUserId(userId)).toRight(UserNotFoundError())

  def getByCredentials(
                        email: String, password: String, deviceType: String
                      ): EitherT[F, ValidationError, Credentials] =
    for {
      creds <- rpos.credentialsRepo().findByCreds(email, deviceType).toRight(UserAuthenticationFailedError(email))
      passwordMatched <- isPasswordMatch(password, creds)
    } yield passwordMatched

  def resetPassword(
                     email: String, deviceType: String
                   ): EitherT[F, ValidationError, Credentials] =
    for {
      creds <- rpos.credentialsRepo().findByCreds(email, deviceType).toRight(UserAuthenticationFailedError(email))
      newPassword <- EitherT.pure(conversions.generateRandomString(10))
      _ <- updatePassword(creds._id.toHexString, newPassword).toRight(CouldNotUpdatePasswordError())
      _ <- EitherT.pure(rpos.emailConf().send(email, "Your new password", newPassword))
    } yield creds


  private def isPasswordMatch(password: String, creds: Credentials): EitherT[F, ValidationError, Credentials] = {
    val passwordMatch =
      if (password.isBcrypted(creds.password)) {
        Right(creds)
      } else {
        Left(UserAuthenticationFailedError(creds.email))
      }
    EitherT.fromEither(passwordMatch)
  }

  def update(user: UserT): EitherT[F, UserNotFoundError, UserT] =
    for {
      saved <- rpos.usersRepo().update(user).toRight(UserNotFoundError())
    } yield saved


  def getAndroidPurchase(request: AndroidReceiptValidationRequest): EitherT[F, ValidationError, domain.SubscriptionPurchase] = {
    getAndroidPurchase(request.productId, request.token)
  }

  def getAndroidPurchase(productId: String, token: String): EitherT[F, ValidationError, domain.SubscriptionPurchase] = {
    EitherT.pure({
      rpos.androidConf().purchases().subscriptions().get(ApplicationConfig.PACKAGE_NAME, productId, token).execute().into[domain.SubscriptionPurchase].enableBeanGetters
        .withFieldComputed(_.expiryTimeMillis, _.getExpiryTimeMillis.toLong)
        .withFieldComputed(_.startTimeMillis, _.getStartTimeMillis.toLong).transform
    })
  }

  def getApplePurchase(receiptData: String, password: String): EitherT[F, ValidationError, AppleSubscriptionPurchase] = {
    implicit val backend = HttpURLConnectionBackend()

    val json = ApplePurchaseVerificationRequest(receiptData, password, true).asJson.toString()
    val req = sttp.body(json).contentType("application/json")
      .post(uri"https://sandbox.itunes.apple.com/verifyReceipt")

    EitherT.fromEither(
      req
        .send().body
        .left.map(UnknownError(_))
        .flatMap(s=>{
          println(s)
          parser.parse(s)
        })
        .map(root.receipt.in_app.each.json.getAll(_))
        .flatMap(_.map(p => p.as[AppleSubscriptionPurchase])
          .filter(_.isRight).maxBy(_.right.get.expires_date_ms))
        .left.map(e => UnknownError(e.getMessage))
    )
  }

  def createFeedback(feedback: Feedback): EitherT[F, ValidationError, Feedback] = {
    EitherT.liftF(rpos.feedbackRepository.create(feedback))
  }

  def updateTrialUsers() = {
    for {
      users <- rpos.usersRepo().findTrialExpiredUsers()
      _ <- convert(users.map(user => makeUserTrialExpired(user)).toList)
    } yield ()
  }

  def updateAndroidSubscribedUsers() = {
    for {
      users <- rpos.usersRepo().findAndroidPremiumExpiredUsers()
      _ <- convert(users.map(user=>updateAndroidSubscribedUser(UserId(user._id.toHexString))).toList)
    } yield ()
  }

  private def updateAndroidSubscribedUser(user:UserId ) = {
    for {
      token <- rpos.androidPurchaseRepo().getLastForUser(user.id)
      purchase <- getAndroidPurchase(token.subscriptionId, token.purchaseToken)
      _ <- updateSubscribedUserRole(user, purchase.startTimeMillis, purchase.expiryTimeMillis)
    } yield ()
  }

  def updateAppleSubscribedUsers() = {
    for {
      users <- rpos.usersRepo().findApplePremiumExpiredUsers()
      _ <- convert(users.map(user=>updateAppleSubscribedUser(UserId(user._id.toHexString))).toList)
    } yield ()
  }

  private def updateAppleSubscribedUser(user:UserId ) = {
    for {
      token <- rpos.applePurchaseRepo().getLastForUser(user.id)
      purchase <- getApplePurchase(token.purchaseToken, secrets.read.surfsUp.apple.appSecret)
      _ <- updateSubscribedUserRole(user, purchase.purchase_date_ms, purchase.expires_date_ms)
    } yield ()
  }


  private def makeUserTrialExpired(eitherUser: UserT): EitherT[F, ValidationError, UserT] = {
    for {
      updated <- update(eitherUser.copy(userType = UserType.TrialExpired.value, lastPaymentAt = -1, nextPaymentAt = -1))
      _ <- EitherT.liftF(rpos.alertsRepository().disableAllButOneAlerts(updated._id.toHexString))
    } yield updated
  }

  def convert[A](list: List[EitherT[F, ValidationError, A]]) = {
    import cats.implicits._

    type Stack[A] = EitherT[F, ValidationError, A]

    val eitherOfList: Stack[List[A]] = list.sequence
    eitherOfList
  }

}

object UserService {
  def apply[F[_] : Sync](
                          repos: Repos[F],
                        ): UserService[F] =
    new UserService(repos)
}