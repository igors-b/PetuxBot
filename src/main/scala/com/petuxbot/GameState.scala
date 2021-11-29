package com.petuxbot

import cats.effect.Sync
import com.petuxbot.domain._
import com.petuxbot.domain.cardContainers.{Board, Deck, DiscardPile}

final case class GameState (
  deck: Deck = Deck.Empty,
  board: Board = Board.Empty,
  discardPile: DiscardPile = DiscardPile.Empty,
  players: List[Player] = List.empty,
  whoseTurn: Option[Player] = None,
  trumpCard: Option[Card] = None
)

object GameState {

  def make[F[_]: Sync](players: List[Player]): F[GameState] = {
    Sync[F].delay {
      GameState(players = players)
    }
  }
}