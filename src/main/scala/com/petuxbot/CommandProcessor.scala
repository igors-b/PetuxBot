package com.petuxbot

import cats.effect.Sync
import cats.implicits._
import cats.effect.syntax.all._
import com.petuxbot.domain.{GameState}
import com.petuxbot.Response._


trait Command
case object Command {
  object StartGame extends Command
  object DealCard extends Command
}

object CommandProcessor {
  def process[F[_]: Sync](gameState: GameState[F], command: Command): F[(GameState[F], Response)] = {
    val newGameState =
    command match {
      case Command.DealCard => ???
      case Command.StartGame => gameState.startGame() // gameService.startGame()
      case _ => ???
    }

    for {
      state <- newGameState
    } yield (state, OK)


  }
}
