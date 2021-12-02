package com.petuxbot.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot
import com.petuxbot.Command._
import com.petuxbot.{Command, GameState, Response}
import com.petuxbot.Response._
import com.petuxbot.domain.cardContainers.Hand

trait GameService[F[_]]{
  def process(cmd: Command): F[Response]
}

object GameService {

  def of[F[_]: Sync]: F[GameService[F]] =
    Ref.of(GameState()).map(apply)

  def apply[F[_]](
    state: Ref[F, GameState]
  ): GameService[F] =
    new GameService[F] {
      def process(cmd: Command): F[Response] =
        cmd match {

          case AddPlayers(playersToAdd) =>
            state.modify(state => {
              val newState = state.copy(players = state.players ++ playersToAdd)
              (newState, OK)
            })

//          case DealCard => ???

          case StartGame(playerId, deck) =>
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

          case WrongCommand => state.modify(state => (state, Error("Wrong command entered")))
          case _ => ???
        }

    }

}