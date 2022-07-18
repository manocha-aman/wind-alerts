package com.uptech.windalerts.core.alerts

import cats.effect.Sync
import cats.implicits._
import cats.mtl.Raise
import cats.{Applicative, Monad}
import com.uptech.windalerts.core.alerts.domain.Alert
import com.uptech.windalerts.core.user.{UserId, UserType}
import com.uptech.windalerts.core.{AlertNotFoundError, Infrastructure, OperationNotAllowed}

import scala.language.postfixOps

object AlertsService {
  def createAlert[F[_] : Sync](userId: UserId, userType: UserType, alertRequest: AlertRequest)(implicit infrastructure: Infrastructure[F], ONA: Raise[F, OperationNotAllowed]): F[Alert] = {
    for {
      _ <- if (userType.isPremiumUser()) Applicative[F].pure(()) else ONA.raise(OperationNotAllowed(s"Please subscribe to perform this action"))
      created <- infrastructure.alertsRepository.create(alertRequest, userId.id)
    } yield created
  }

  def update[F[_] : Sync](alertId: String, userId: UserId, userType: UserType, alertRequest: AlertRequest)(implicit infrastructure: Infrastructure[F], ONA: Raise[F, OperationNotAllowed], ANF: Raise[F, AlertNotFoundError]): F[Alert] = {
    for {
      _ <- authorizeAlertEditRequest(userId, userType, alertId, alertRequest)
      updated <- infrastructure.alertsRepository.update(userId.id, alertId, alertRequest)
    } yield updated
  }

  def authorizeAlertEditRequest[F[_] : Sync](userId: UserId, userType: UserType, alertId: String, alertRequest: AlertRequest)(implicit infrastructure: Infrastructure[F], FR: Raise[F, OperationNotAllowed], ONA: Raise[F, AlertNotFoundError], A: Applicative[F]): F[Unit] = {
    if (userType.isPremiumUser())
      A.pure(())
    else {
      authorizeAlertEditNonPremiumUser(userId, alertId, alertRequest)
    }
  }

  private def authorizeAlertEditNonPremiumUser[F[_] : Sync](userId: UserId, alertId: String, alertRequest: AlertRequest)(implicit infrastructure: Infrastructure[F], M: Monad[F], ONA: Raise[F, OperationNotAllowed], ANF: Raise[F, AlertNotFoundError], A: Applicative[F]): F[Unit] = {
    for {
      firstAlert <- getFirstAlert(userId)
      canEdit <- if (checkForNonPremiumUser(alertId, firstAlert, alertRequest)) A.pure(()) else ONA.raise(OperationNotAllowed(s"Please subscribe to perform this action"))
    } yield canEdit
  }

  private def getFirstAlert[F[_] : Sync](userId: UserId)(implicit infrastructure: Infrastructure[F], FR: Raise[F, OperationNotAllowed]) = {
    infrastructure.alertsRepository.getFirstAlert(userId.id).getOrElseF(FR.raise(OperationNotAllowed("Please subscribe to perform this action")))
  }

  def checkForNonPremiumUser[F[_] : Sync](alertId: String, alert: Alert, alertRequest: AlertRequest)(implicit infrastructure: Infrastructure[F], FR: Raise[F, AlertNotFoundError]) = {
    if (alert.id != alertId) {
      false
    } else {
      alert.allFieldExceptStatusAreSame(alertRequest)
    }
  }

  def getAllForUser[F[_] : Sync](user: String)(implicit infrastructure: Infrastructure[F]): F[Seq[Alert]] = infrastructure.alertsRepository.getAllForUser(user)

  def delete[F[_] : Sync](requester: String, alertId: String)(implicit infrastructure: Infrastructure[F], FR: Raise[F, AlertNotFoundError]) = {
    infrastructure.alertsRepository.delete(requester, alertId)
  }
}


