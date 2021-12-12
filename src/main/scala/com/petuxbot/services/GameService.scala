package com.petuxbot.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot.BotData.BotId
import com.petuxbot.Request._
import com.petuxbot.{Request, Response}
import com.petuxbot.Response._
import com.petuxbot.domain.cardcontainers._
import com.petuxbot.game.GameError._
import com.petuxbot.domain.{Score, StrongestCard}
import com.petuxbot.game.GameState
import com.petuxbot.game.CardValidator._


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

          case BotMakesAttack(playerId) =>
            state.modify(oldState => {
              val players = oldState.players
              val deck = oldState.deck
              val board = oldState.board
              val botOpt = players.find(_.id == BotId)
              val gameState = for {
                bot              <- botOpt
                others           =  players.diff(List(bot))
                strongestCard    <- board.strongestCard
                cardToHit        <- board.cardToHit
                card             <- bot.cards.find(card => isCardValidToMakeAttack(card, bot, strongestCard, cardToHit))
                updatedBot       =  bot.copy(hand = bot.hand.removeCard(card))
                updatedPlayers   =  others :+ updatedBot
                newStrongestCard =  if (isCardStronger(card, strongestCard.value)) StrongestCard(card, BotId)
                                    else strongestCard
                newBoard         =  board.addCard(card).copy(strongestCard = Some(newStrongestCard))
                winner           <- updatedPlayers.find(_.id == newStrongestCard.ownerId)
                winnerWithAddedScore = winner.copy(score = Score(winner.score.value -1))
                winnerWithAddedTricks = winnerWithAddedScore.copy(tricks = winnerWithAddedScore.tricks :+ Trick(newBoard.cards))
                newOthers        =  updatedPlayers.diff(List(winner))
                newPlayers       =  newOthers :+ winnerWithAddedTricks

              } yield GameState(
                deck        = deck,
                board       = newBoard,
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = players.find(_.id == winnerWithAddedTricks.id),
                players     = newPlayers
              )

              gameState match {
                case Some(newState) => newState.players.find(_.id == playerId) match {

                  case Some(player) =>
                    val playerCardsLeft = newState.players.map(_.cards.size)
                    val isEmptyHands = playerCardsLeft.contains(0)
                    val gameScores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                    val continueRound =
                      (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, gameScores))
                    val endRound =
                      (newState, ShowTotalsToPlayer(newState.board, gameScores))
                    if (isEmptyHands) endRound
                    else continueRound
                  case None         => (oldState, Error(WrongPlayerId))
                }
                case None           =>  (oldState, Error(WrongCard))
              }
            })

          case BotMakesTurn(playerId) =>
            state.modify(oldState => {
              val players = oldState.players
              val deck = oldState.deck
              val board = Board.Empty
              val botOpt = players.find(_.id == BotId)
              val scores     = players.map(_.score)
              val gameState = for {
                bot              <- botOpt
                card             <- bot.cards.find(card => isCardValidToMakeTurn(card, bot, scores))
                others           =  players.diff(List(bot))
                updatedBot       =  bot.copy(hand = bot.hand.removeCard(card))
                updatedPlayers   =  others :+ updatedBot
              } yield GameState(
                deck = deck,
                board = board.addCard(card).setCardToHit(card).setStrongestCard(card, BotId),
                discardPile = oldState.discardPile,
                trumpCard = oldState.trumpCard,
                whoseTurn = players.find(_.id == playerId),
                players = updatedPlayers
              )

              gameState match {
                case Some(newState) => newState.players.find(_.id == playerId) match {
                  case Some(player) =>
                    val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                    (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))

                  case None         => (oldState, Error(WrongPlayerId))
                }
                case None           =>  (oldState, Error(WrongCard))
              }
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
                case None         => (newState, Error(WrongPlayerId))
              }
            })

          case GetPlayerIdWhoseTurn =>
            state.modify(state => {
              state.whoseTurn match {
                case Some(player) => (state, WhoseTurn(player.id))
                case None         => (state, Error(WhoseTurnNotSet))
              }
            })

          case PlayerMakesTurn(playerId, card) =>
            state.modify(oldState => {
              val players     = oldState.players
              val scores      = players.map(_.score)
              val board       = Board.Empty
              val playerOpt   = players.find(_.id == playerId)
              val gameState   = for {
                player        <- playerOpt
                if isCardValidToMakeTurn(card, player, scores)
                others        =  players.diff(List(player))
                updatedPlayer =  player.copy(hand = player.hand.removeCard(card))
              } yield GameState(
                  deck        = Deck.Empty,
                  board       = board.addCard(card).setCardToHit(card).setStrongestCard(card, playerId),
                  discardPile = oldState.discardPile.addCards(oldState.deck.cards),
                  trumpCard   = oldState.trumpCard,
                  whoseTurn   = players.find(_.id == 0),
                  players     = updatedPlayer +: others
              )

              gameState match {
                case Some(newState) => newState.players.find(_.id == playerId) match {
                  case Some(player) =>
                    val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                    (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))
                  case None         => (oldState, Error(WrongPlayerId))
                }
                case None           => (oldState, Error(WrongCard))
              }
            })

          case PlayerMakesAttack(playerId, card) =>
            state.modify(oldState => {
              val players = oldState.players
              val deck = oldState.deck
              val board = oldState.board
              val playerOpt = players.find(_.id == playerId)
              val gameState = for {
                player                <- playerOpt
                others                =  players.diff(List(player))
                strongestCard         <- board.strongestCard
                cardToHit             <- board.cardToHit
                if isCardValidToMakeAttack(card, player, strongestCard, cardToHit)
                updatedPlayer         =  player.copy(hand = player.hand.removeCard(card))
                updatedPlayers        =  others :+ updatedPlayer
                newStrongestCard      =  if (isCardStronger(card, strongestCard.value)) StrongestCard(card, playerId)
                                        else strongestCard
                newBoard              =  board.addCard(card).copy(strongestCard = Some(newStrongestCard))
                winner                <- updatedPlayers.find(_.id == newStrongestCard.ownerId)
                winnerWithAddedScore  =  winner.copy(score = Score(winner.score.value -1))
                winnerWithAddedTricks =  winnerWithAddedScore.copy(tricks = winnerWithAddedScore.tricks :+ Trick(newBoard.cards))
                newOthers             =  updatedPlayers.diff(List(winner))
                newPlayers            =  newOthers :+ winnerWithAddedTricks

              } yield GameState(
                deck        = deck,
                board       = newBoard,
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = players.find(_.id == winnerWithAddedTricks.id),
                players     = newPlayers
              )

              gameState match {
                case Some(newState) => newState.players.find(_.id == playerId) match {
                  case Some(player) =>
                    val playerCardsLeft = newState.players.map(_.cards.size)
                    val isEmptyHands = playerCardsLeft.contains(0)
                    val gameScores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                    val continueRound =
                    (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, gameScores))
                    val endRound =
                      (newState, ShowTotalsToPlayer(newState.board, gameScores))
                      if (isEmptyHands) endRound
                      else continueRound

                  case None         => (oldState, Error(WrongPlayerId))
                }
                case None           =>  (oldState, Error(WrongCard))
              }
            })

          case StartRound(playerId, deck) =>
            state.modify(oldState => {
              val players   = oldState.players
              val hands     = players.map(_ => Hand.Empty)
              val trumpCard = deck.cards.lastOption
              val gameState =
                for {
                  (deck, dealtHands)    <- deck.deal(hands)
                  whoseTurn             = players.headOption
                  playersWithDealtHands = players zip dealtHands map {
                    case (player, newHand) => player.addCardsToHand(newHand.cards)
                  }
                } yield GameState(
                  deck = deck,
                  players = playersWithDealtHands,
                  trumpCard = trumpCard,
                  whoseTurn = whoseTurn
                )

              val newState = gameState.getOrElse(oldState)

              newState.players.find(_.id == playerId) match {
                case Some(player) =>
                  val scores = newState.players.map(player => s"${player.name}: ${player.score.value}")
                  (newState, ShowBoardAndHandToPlayer(newState.board, player.hand, newState.trumpCard, scores))
                case None         => (newState, Error(WrongPlayerId))
              }
            })

          case ResolveRound =>
            state.modify(oldState => {
              val deck        = oldState.deck
              val board       = Board.Empty
              val trumpCard   = oldState.trumpCard
              val players     = oldState.players.map {
                player =>
                if (player.tricks.isEmpty) player.copy(score = Score(player.score.value + 5))
                else player
              }
              val gameState = GameState(
                deck        = deck,
                board       = board,
                discardPile = oldState.discardPile,
                trumpCard   = trumpCard,
                whoseTurn   = None,
                players     = players
              )

              val scores = players.map(player => s"${player.name}: ${player.score.value}")

              (gameState, ShowTotalsToPlayer(board, scores))
            })
        }
    }
}