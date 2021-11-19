package com.petuxbot

import cats.effect.Sync

object Parser {

  def parse[F[_]: Sync](cmd: String): F[Command] = ???
}
