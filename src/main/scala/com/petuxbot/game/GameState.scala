package com.petuxbot.game

import com.petuxbot.domain._
import com.petuxbot.domain.cardcontainers.{Board, Deck, DiscardPile}

final case class GameState (
  deck: Deck = Deck.Empty,
  board: Board = Board.Empty,
  discardPile: DiscardPile = DiscardPile.Empty,
  players: List[Player] = List.empty,
  whoseTurn: Option[Player] = None,
  trumpCard: Option[Card] = None
)
