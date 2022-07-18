package com.uptech.windalerts.core.beaches

import cats.mtl.Raise
import com.uptech.windalerts.core.BeachNotFoundError
import com.uptech.windalerts.core.beaches.domain._

trait WindStatusProvider[F[_]] {
  def get(beachId: BeachId)(implicit FR: Raise[F, BeachNotFoundError]): F[Wind]
}