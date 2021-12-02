package com.petuxbot.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot
import com.petuxbot.Request._
import com.petuxbot.{GameState, Request, Response}
import com.petuxbot.Response._
import com.petuxbot.domain.cardContainers.Hand

trait GameService[F[_]]{
  def process(request: Request): F[Response]
}

object GameService {

  def of[F[_]: Sync]: F[GameService[F]] =
    Ref.of(GameState()).map(apply)

  def apply[F[_]](
    state: Ref[F, GameState]
  ): GameService[F] =
    new GameService[F] {
      def process(request: Request): F[Response] =
        request match {

          case AddPlayers(playersToAdd) =>
            state.modify(state => {
              val newState = state.copy(players = state.players ++ playersToAdd)
              (newState, OK)
            })

          case ChangeCardsForPlayer(playerId, cards) =>
            state.modify(oldState => {
              val players = oldState.players
              val deck = oldState.deck
              val playerOpt = players.find(_.id == playerId)
              val result: Option[GameState] = for {
                player                <- playerOpt
                pwrc                  =  player.removeCardsFromHand(cards)
                otherPlayers          =  players.diff(List(player))
                (newDeck, dealtHands) <- deck.deal(List(pwrc.hand))
                playerWithDealtHand   =  List(pwrc) zip dealtHands map {
                  case (player, newHand) => player.copy(hand = newHand)
                }
              } yield petuxbot.GameState(
                  deck = newDeck,
                  board = oldState.board,
                  discardPile = oldState.discardPile,
                  whoseTurn = oldState.whoseTurn,
                  trumpCard = oldState.trumpCard,
                  players = playerWithDealtHand ++ otherPlayers
              )

              val newState = result.getOrElse(oldState)

              newState.players.find(_.id == playerId) match {
                case Some(player) => (newState, ShowCardsToPlayer(player.hand.cards, newState.trumpCard))
                case None         => (newState, Error("Player with such Id not found"))
              }
            })

          case StartRound(playerId, deck) =>
            state.modify(oldState => {
              val players = oldState.players
              val hands = players.map(_ => Hand.Empty)
              val trumpCard = deck.cards.lastOption
              val result: Option[GameState] = for {
                (deck, dealtHands) <- deck.deal(hands)
                whoseTurn = players.headOption
                playersWithDealtHands = players zip dealtHands map {
                  case (player, newHand) => player.addCardsToHand(newHand.cards)
                }
              } yield petuxbot.GameState(deck = deck, players = playersWithDealtHands, trumpCard = trumpCard, whoseTurn = whoseTurn)

              val newState = result.getOrElse(oldState)

              newState.players.find(_.id == playerId) match {
                case Some(player) => (newState, ShowCardsToPlayer(player.hand.cards, newState.trumpCard))
                case None         => (newState, Error("Player with such Id not found"))
              }
            })

          case WrongRequest => state.modify(state => (state, Error("Wrong command entered")))
          case _ => ???
        }

    }

}