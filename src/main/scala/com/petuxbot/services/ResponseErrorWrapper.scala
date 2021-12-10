package com.petuxbot.services

import cats.effect.Sync
import com.petuxbot.Response

trait ResponseErrorWrapper[F[_]] {
  def apply(response: Response): F[Response]
}

object ResponseErrorWrapper {
  def apply[F[_]: Sync] : ResponseErrorWrapper[F] = new ResponseErrorWrapper[F] {
    def apply(response: Response): F[Response] = Sync[F].delay(response)
  }
}


