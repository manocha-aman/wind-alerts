package com.uptech.windalerts.core

import com.uptech.windalerts.core.alerts.AlertsRepository
import com.uptech.windalerts.core.beaches.{SwellStatusProvider, TidesStatusProvider, WindStatusProvider}
import com.uptech.windalerts.core.notifications.{NotificationRepository, NotificationsSender}
import com.uptech.windalerts.core.social.SocialPlatformType
import com.uptech.windalerts.core.social.login.SocialLoginProvider
import com.uptech.windalerts.core.user.{PasswordNotifier, UserRepository}
import com.uptech.windalerts.core.user.credentials.{CredentialsRepository, SocialCredentialsRepository}
import com.uptech.windalerts.core.user.sessions.UserSessionRepository

case class BeachesInfrastructure[F[_]](windStatusProvider: WindStatusProvider[F],
                                       tideStatusProvider: TidesStatusProvider[F],
                                       swellStatusProvider: SwellStatusProvider[F])

case class Infrastructure[F[_]](
                                 jwtKey: String,
                                 userRepository: UserRepository[F],
                                 userSessionsRepository: UserSessionRepository[F],
                                 socialCredentialsRepositories: Map[SocialPlatformType, SocialCredentialsRepository[F]],
                                 socialLoginProviders: Map[SocialPlatformType, SocialLoginProvider[F]],
                                 credentialsRepository: CredentialsRepository[F],
                                 eventPublisher: EventPublisher[F],
                                 passwordNotifier: PasswordNotifier[F],
                                 alertsRepository: AlertsRepository[F]
                               )
case class NotificationInfrastructure[F[_]](
                                             beachesInfrastructure:BeachesInfrastructure[F],
                                             alertsRepository: AlertsRepository[F],
                                             userRepository: UserRepository[F],
                                             userSessionsRepository: UserSessionRepository[F],
                                             notificationsRepository: NotificationRepository[F],
                                             notificationSender: NotificationsSender[F]
                               )