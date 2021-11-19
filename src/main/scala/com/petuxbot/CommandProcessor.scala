package com.petuxbot

import cats.effect.Sync
import cats.implicits._
import com.petuxbot.domain.GameState
import com.petuxbot.Response._


trait Command
case object Command {
  object StartGame extends Command
  object WrongCommand extends Command
  object DealCard extends Command
}

object CommandProcessor {
  def process[F[_]: Sync](gameState: GameState[F], command: Command): F[(GameState[F], Response)] = {

    command match {
      case Command.DealCard => ???
      case Command.StartGame => gameState.startGame().map {
        case Some(newState) => (newState, OK)
        case None => (gameState, Error("Dealing cards failed"))
      }
      case _ => Sync[F].delay(gameState, Error("You entered wrong command"))
    }
  }
}
