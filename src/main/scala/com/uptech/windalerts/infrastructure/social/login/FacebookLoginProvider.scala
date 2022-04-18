package com.uptech.windalerts.infrastructure.social.login

import cats.Monad
import cats.effect.{Async, ContextShift}
import com.restfb.types.User
import com.restfb.{DefaultFacebookClient, Parameter, Version}
import com.uptech.windalerts.core.social.login.{SocialLoginProvider, SocialUser}
import com.uptech.windalerts.infrastructure.social.login.AccessRequests.FacebookRegisterRequest

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class FacebookLoginProvider[F[_]](fbSecret: String)(implicit cs: ContextShift[F], s: Async[F], M: Monad[F])  extends SocialLoginProvider[F, FacebookRegisterRequest] {
  override def fetchUserFromPlatform(credentials: FacebookRegisterRequest): F[SocialUser] = {
    fetchUserFromPlatform_(credentials)
  }

  private def fetchUserFromPlatform_(credentials: FacebookRegisterRequest) = {
    Async.fromFuture(M.pure(Future.successful(new DefaultFacebookClient(credentials.accessToken, fbSecret, Version.LATEST))
      .flatMap(client => Future(client.fetchObject("me", classOf[User], Parameter.`with`("fields", "name,id,email")))
        .map(facebookUser => SocialUser(facebookUser.getId, facebookUser.getEmail, credentials.deviceType, credentials.deviceToken, facebookUser.getFirstName)))))
  }

}