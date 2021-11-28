package com.petuxbot.domain

import cats.effect.Sync

final case class GameState (
  deck: Deck = Deck.make,
  board: Board = Board.Empty,
  discardPile: DiscardPile = DiscardPile.Empty,
  players: List[Player],
  whoseTurn: Option[Player] = None
)

object GameState {

  def make[F[_]: Sync](players: List[Player]): F[GameState] = {
    Sync[F].delay {
      GameState(players = players)
    }
  }

  def startGame[F[_]: Sync](players: List[Player]): F[Option[GameState]] = {
    val hands = players.map(_ => Hand.Empty)
    val result: Option[GameState] = for {
      (deck, dealtHands) <- Deck.make.deal(hands)
      whoseTurn = players.headOption
      playersWithDealtHands = players zip dealtHands map {
        case (player, newHand) => player.addCardsToHand(newHand.cards)
      }
    } yield GameState(deck, Board.Empty, DiscardPile.Empty, playersWithDealtHands, whoseTurn)

    Sync[F].delay(result)
  }
}