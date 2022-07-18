package com.uptech.windalerts.core.notifications

import cats.effect.{Async, Sync}
import cats.implicits._
import cats.mtl.Raise
import cats.{Monad, Parallel}
import com.uptech.windalerts.core.alerts.domain.Alert
import com.uptech.windalerts.core.beaches.Beaches
import com.uptech.windalerts.core.beaches.domain._
import com.uptech.windalerts.core.notifications.NotificationsSender.NotificationDetails
import com.uptech.windalerts.core.user.{UserId, UserT}
import com.uptech.windalerts.core.{BeachNotFoundError, NotificationInfrastructure}
import com.uptech.windalerts.logger

object NotificationsService {
  final case class UserDetails(userId: String, email: String)

  final case class AlertWithBeach(alert: Alert, beach: Beach)

  final case class UserDetailsWithDeviceToken(userId: String, email: String, deviceToken: String, notificationsPerHour: Long)

  final case class AlertWithUserWithBeach(alert: Alert, user: UserDetailsWithDeviceToken, beach: Beach)


  def sendNotification[F[_] : Sync : Parallel]()(implicit infrastructure: NotificationInfrastructure[F], F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    for {
      alertWithUserWithBeach <- findAllAlertsToNotify()
      submitted <- alertWithUserWithBeach.map(submit(_)).toList.sequence
    } yield submitted
  }

  def findAllAlertsToNotify[F[_] : Sync : Parallel]()(implicit  infrastructure: NotificationInfrastructure[F],F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    for {
      usersReadyToReceiveNotifications <- allLoggedInUsersReadyToReceiveNotifications()
      alertsByBeaches <- alertsForUsers(usersReadyToReceiveNotifications)
      beaches <- beachStatuses(alertsByBeaches.keys.toSeq)
      alertsToBeNotified = alertsByBeaches
        .map(kv => (beaches(kv._1), kv._2))
        .map(kv => (kv._1, kv._2.filter(_.isToBeNotified(kv._1)).map(AlertWithBeach(_, kv._1))))
      _ <- F.delay(logger.info(s"alertsToBeNotified : ${alertsToBeNotified.values.map(_.flatMap(_.alert.id)).mkString(", ")}"))
      userIdToUser = usersReadyToReceiveNotifications.map(u => (u.userId, u)).toMap
      alertWithUserWithBeach = alertsToBeNotified.values.flatten.map(v => AlertWithUserWithBeach(v.alert, userIdToUser(v.alert.owner), v.beach))
    } yield alertWithUserWithBeach
  }


  private def allLoggedInUsersReadyToReceiveNotifications[F[_] : Sync : Parallel]()(implicit  infrastructure: NotificationInfrastructure[F], F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    for {
      usersWithNotificationsEnabledAndNotSnoozed <- infrastructure.userRepository.findUsersWithNotificationsEnabledAndNotSnoozed()
      _ <- F.delay(logger.info(s"usersWithNotificationsEnabledAndNotSnoozed : ${usersWithNotificationsEnabledAndNotSnoozed.map(_.id).mkString(", ")}"))

      loggedInUsers <- filterLoggedOutUsers(usersWithNotificationsEnabledAndNotSnoozed)

      usersWithLastHourNotificationCounts <- loggedInUsers.map(u => infrastructure.notificationsRepository.countNotificationsInLastHour(u.userId)).toList.sequence
      zipped = loggedInUsers.zip(usersWithLastHourNotificationCounts)

      usersReadyToReceiveNotifications = zipped.filter(u => u._2.count < u._1.notificationsPerHour).map(_._1)
      _ <- F.delay(logger.info(s"usersReadyToReceiveNotifications : ${usersReadyToReceiveNotifications.map(_.userId).mkString(", ")}"))
    } yield usersReadyToReceiveNotifications
  }

  private def filterLoggedOutUsers[F[_] : Sync : Parallel](usersWithNotificationsEnabledAndNotSnoozed: Seq[UserT])(implicit infrastructure:NotificationInfrastructure[F],  F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    for {
      userSessions <- usersWithNotificationsEnabledAndNotSnoozed.map(u => infrastructure.userSessionsRepository.getByUserId(u.id).value).toList.sequence
      usersWithSession = usersWithNotificationsEnabledAndNotSnoozed.zip(userSessions)
      loggedInUsers = usersWithSession.filter(_._2.isDefined).map(u => UserDetailsWithDeviceToken(u._1.id, u._1.email, u._2.get.deviceToken, u._1.notificationsPerHour))
    } yield loggedInUsers
  }

  private def alertsForUsers[F[_] : Sync : Parallel](users: Seq[UserDetailsWithDeviceToken])(implicit infrastructure:NotificationInfrastructure[F], F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    for {
      alertsForUsers <- users.map(u => infrastructure.alertsRepository.getAllEnabledForUser(u.userId)).sequence.map(_.flatten)
      alertsForUsersWithMatchingTime = alertsForUsers.toList.filter(_.isTimeMatch())
      alertsByBeaches = alertsForUsersWithMatchingTime.groupBy(_.beachId).map(kv => (BeachId(kv._1), kv._2))
    } yield alertsByBeaches
  }

  private def beachStatuses[F[_] : Sync : Parallel](beachIds: Seq[BeachId])(implicit infrastructure:NotificationInfrastructure[F],  F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]) = {
    implicit val beachesInfrastructure = infrastructure.beachesInfrastructure
    Beaches.getAll(beachIds)
  }

  private def submit[F[_] : Sync : Parallel](u: AlertWithUserWithBeach)(implicit notificationInfrastructure: NotificationInfrastructure[F], F: Async[F],  FR: Raise[F, BeachNotFoundError], M:Monad[F]): F[Unit] = {
    for {
      status <- notificationInfrastructure.notificationSender.send(NotificationDetails(BeachId(u.alert.beachId), u.user.deviceToken, UserId(u.user.userId)))
      _ <- F.delay(logger.info(s"Notification response to ${u.user.email} is ${status}"))
      _ <- notificationInfrastructure.notificationsRepository.create(u.alert.id, u.user.userId, u.user.deviceToken, System.currentTimeMillis())
    } yield ()
  }

}
