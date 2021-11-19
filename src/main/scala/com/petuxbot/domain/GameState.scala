package com.petuxbot.domain

import cats.effect.Sync

final case class GameState[F[_]: Sync] (
  deck: Deck = Deck.make,
  board: Board = Board.empty,
  discardPile: DiscardPile = DiscardPile.empty,
  players: Vector[Player],
  whoseTurn: Option[Player] = None
){

  def startGame(): F[Option[GameState[F]]] = {
    val hands = players.map(_ => Hand.empty)
    val result: Option[GameState[F]] = for {
      (deck, dealtHands) <- Deck.make.deal(hands)
      whoseTurn = players.headOption
      playersWithDealtHands = players zip dealtHands map {
        case (player, newHand) => player.addCardsToHand(newHand.cards)
      }
    } yield GameState[F](deck, Board.empty, DiscardPile.empty, playersWithDealtHands, whoseTurn)

    Sync[F].delay(result)
  }
}

object GameState {

  def make[F[_]: Sync](players: Vector[Player]): F[GameState[F]] = {
    Sync[F].delay {
      GameState(players = players)
    }
  }
}