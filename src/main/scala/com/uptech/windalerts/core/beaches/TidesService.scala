package com.uptech.windalerts.core.beaches

import cats.Functor
import com.uptech.windalerts.core.SurfsUpError
import com.uptech.windalerts.core.beaches.domain.{BeachId, TideHeight}
import com.uptech.windalerts.infrastructure.endpoints.dtos

trait TidesService[F[_]] {
  def get(beachId: BeachId): cats.data.EitherT[F, SurfsUpError, TideHeight]
}
