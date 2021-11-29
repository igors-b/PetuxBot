package com.petuxbot.domain

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot.Command
import com.petuxbot.Command._
import com.petuxbot.Response
import com.petuxbot.Response._

trait GameService[F[_]]{
  def process(cmd: Command): F[Response]
}

object GameService {

  def of[F[_]: Sync]: F[GameService[F]] =
    Ref.of(GameState()).map(apply)

  def apply[F[_]](
    state: Ref[F, GameState]
  ) =
    new GameService[F] {
      def process(cmd: Command): F[Response] =
        cmd match {

          case AddPlayer(player) =>
            state.modify(state => {
              val newState = state.copy(players = state.players :+ player)
              (newState, OK)
            })

          case DealCard => ???
          case StartGame =>
            state.modify(oldState => {
              val players = oldState.players
              val hands = players.map(_ => Hand.Empty)
              val deck = Deck.make
              val trumpCard = deck.cards.lastOption
              val result: Option[GameState] = for {
                (deck, dealtHands) <- deck.deal(hands)
                whoseTurn = players.headOption
                playersWithDealtHands = players zip dealtHands map {
                  case (player, newHand) => player.addCardsToHand(newHand.cards)
                }
              } yield GameState(deck = deck, players = playersWithDealtHands, trumpCard = trumpCard, whoseTurn = whoseTurn)

              val newState = result.getOrElse(oldState)
              (newState, ShowCardsToPlayer(newState.players.head.hand.cards, newState.deck.cards.last)) //TODO error handling required
            })

          case ResetState =>
            state.modify(_ => {
              (GameState(), OK)
            })

          case WrongCommand => state.modify(state => (state, Error("Error")))
          case _ => ???
        }

    }

}