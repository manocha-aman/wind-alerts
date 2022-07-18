package com.uptech.windalerts.core.beaches

import cats.mtl.Raise
import com.uptech.windalerts.core.{BeachNotFoundError, SurfsUpError}
import com.uptech.windalerts.core.beaches.domain._


trait SwellStatusProvider[F[_]] {
  def get(beachId: BeachId)(implicit FR: Raise[F, BeachNotFoundError]): F[Swell]
}
