package com.petuxbot.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot.Request._
import com.petuxbot.{GameState, Request, Response}
import com.petuxbot.Response._
import com.petuxbot.domain.cardContainers.{Deck, Hand}
import com.petuxbot.CardValidator._


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
                player                   <- playerOpt
                playerAfterCardsRemoved  =  player.removeCardsFromHand(cards)
                otherPlayers             =  players.diff(List(player))
                (newDeck, dealtHands)    <- deck.deal(List(playerAfterCardsRemoved.hand))
                playerWithDealtHand      =  List(playerAfterCardsRemoved) zip dealtHands map {
                  case (player, newHand) => player.copy(hand = newHand)
                }
              } yield GameState(
                  deck        = newDeck,
                  board       = oldState.board,
                  discardPile = oldState.discardPile.addCards(cards),
                  whoseTurn   = oldState.whoseTurn,
                  trumpCard   = oldState.trumpCard,
                  players     = playerWithDealtHand ++ otherPlayers
              )

              val newState = result.getOrElse(oldState)

              newState.players.find(_.id == playerId) match {
                case Some(player) =>
                  val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                  (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))
                case None         => (newState, Error("Player with such Id not found"))
              }
            })

          case GetPlayerIdWhoseTurn =>
            state.modify(state => {
              state.whoseTurn match {
                case Some(player) => (state, WhoseTurn(player.id))
                case None         => ???
              }
            })

          case PlayerMakesTurn(playerId, card) =>
            state.modify(oldState => {
              val players    = oldState.players
              val scores     = players.map(_.score)
              val board      = oldState.board
              val playerOpt  = players.find(_.id == playerId)
              val gameState = for {
                player        <- playerOpt
                if isCardValidToMakeTurn(card, player, scores)
                others        =  players.diff(List(player))
                updatedPlayer =  player.copy(hand = player.hand.removeCard(card))
              } yield GameState(
                deck = Deck.Empty,
                board = board.addCard(card).setCardToHit(card).setStrongestCard(card),
                discardPile = oldState.discardPile.addCards(oldState.deck.cards),
                trumpCard = oldState.trumpCard,
                whoseTurn = players.find(_.id == 0),
                players = updatedPlayer +: others
              )

              gameState match {
                case Some(newState) => newState.players.find(_.id == playerId) match {
                  case Some(player) =>
                    val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                    (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))
                  case None         => (oldState, Error("There is no player with such Id"))
                }
                case None           =>  (oldState, Error("Wrong Card")) //add error
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
              } yield GameState(deck = deck, players = playersWithDealtHands, trumpCard = trumpCard, whoseTurn = whoseTurn)

              val newState = result.getOrElse(oldState)

              newState.players.find(_.id == playerId) match {
                case Some(player) =>
                  val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                  (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))
                case None         => (newState, Error("Player with such Id not found"))
              }
            })

          case WrongRequest => state.modify(state => (state, Error("Wrong command entered")))
          case _ => ???
        }

    }

}