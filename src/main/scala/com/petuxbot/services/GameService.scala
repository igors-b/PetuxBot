package com.petuxbot.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.toFunctorOps
import com.petuxbot.BotData.BotId
import com.petuxbot.Command._
import com.petuxbot.{Command, Response}
import com.petuxbot.Response._
import com.petuxbot.domain.cardcontainers._
import com.petuxbot.game.GameError._
import com.petuxbot.domain.{Score, StrongestCard}
import com.petuxbot.game.{GameError, GameState}
import com.petuxbot.game.CardValidator._


trait GameService[F[_]]{
  def process(command: Command): F[Response]
}

object GameService {

  def of[F[_]: Sync]: F[GameService[F]] =
    Ref.of(GameState()).map(apply)

  def apply[F[_]](
    state: Ref[F, GameState]
  ): GameService[F] =
    new GameService[F] {

      def process(command: Command): F[Response] =
        command match {

          case AddPlayers(playersToAdd) =>
            state.modify(_ => {
              val newState = GameState(players = playersToAdd)
              (newState, OK)
            })

          case BotMakesAttack(playerId) =>
            state.modify(oldState => {
              val players   = oldState.players
              val board     = oldState.board
              val botOpt    = players.find(_.id == BotId)
              val gameState = for {
                bot              <- botOpt.toRight(WrongPlayerId(s"Bot with provided playerId: $BotId not found in game state list of players"))
                others           =  players.diff(List(bot))
                strongestCard    <- board.strongestCard.toRight(UnexpectedGameState("Strongest card not found"))
                cardToHit        <- board.cardToHit.toRight(UnexpectedGameState("Card to hit not found"))
                card             <- bot.cards.find(card => isCardValidToMakeAttack(card, bot, strongestCard, cardToHit)).toRight(WrongCard("Wrong card received for attack"))
                updatedBot       =  bot.copy(hand = bot.hand.removeCard(card))
                updatedPlayers   =  others :+ updatedBot
                newStrongestCard =  if (isCardStronger(card, strongestCard.value)) StrongestCard(card, BotId)
                                    else strongestCard
                newBoard         =  board.addCard(card).copy(strongestCard = Some(newStrongestCard))
                winner           <- updatedPlayers.find(_.id == newStrongestCard.ownerId)
                                      .toRight(WrongPlayerId(s"Winner with provided playerId: ${newStrongestCard.ownerId} not found"))
                winnerWithAddedScore = winner.copy(score = Score(winner.score.value -1))
                winnerWithAddedTricks = winnerWithAddedScore.copy(tricks = winnerWithAddedScore.tricks :+ Trick(newBoard.cards))
                newOthers        =  updatedPlayers.diff(List(winner))
                newPlayers       =  newOthers :+ winnerWithAddedTricks

              } yield GameState(
                deck        = oldState.deck,
                board       = newBoard,
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = Some(winnerWithAddedTricks),
                players     = newPlayers
              )

              resolveRequest(oldState, gameState, playerId)
            })

          case BotMakesTurn(playerId) =>
            state.modify(oldState => {
              val players   = oldState.players
              val board     = Board.Empty
              val botOpt    = players.find(_.id == BotId)
              val scores    = players.map(_.score)
              val gameState = for {
                bot              <- botOpt.toRight(WrongPlayerId(s"Bot with provided playerId: $BotId not found in game state list of players"))
                card             <- bot.cards.find(card => isCardValidToMakeTurn(card, bot, scores)).toRight(CardValidationError("Card validation failed during getting valid card to make bot's turn with card"))
                others           =  players.diff(List(bot))
                updatedBot       =  bot.copy(hand = bot.hand.removeCard(card))
                updatedPlayers   =  others :+ updatedBot
              } yield GameState(
                deck        = oldState.deck,
                board       = board.addCard(card).setCardToHit(card).setStrongestCard(card, BotId),
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = players.find(_.id == playerId),
                players     = updatedPlayers
              )

              resolveRequest(oldState, gameState, playerId)
            })

          case ChangeCardsForPlayer(playerId, cards) =>
            state.modify(oldState => {
              val players   = oldState.players
              val deck      = oldState.deck
              val playerOpt = players.find(_.id == playerId)
              val gameState = for {
                player                   <- playerOpt.toRight(WrongPlayerId(s"Player with provided playerId: $playerId not found in game state list of players"))
                playerAfterCardsRemoved  <- player.removeCardsFromHand(cards).toRight(WrongCardsToChange("Wrong cards to change received from player, there are no such cards in hand"))
                otherPlayers             =  players.diff(List(player))
                eitherOfDeckAndHands     <- deck.deal(List(playerAfterCardsRemoved.hand)).toRight(DealingError("There are no hands to deal cards to."))
                (newDeck, dealtHands)    =  eitherOfDeckAndHands
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

              resolveRequest(oldState, gameState, playerId)
            })

          case GetPlayerIdWhoseTurn =>
            state.modify(state => {
              state.whoseTurn match {
                case Some(player) => (state, WhoseTurn(player.id))
                case None         => (state, Error(UnexpectedGameState("Whose turn not set")))
              }
            })

          case PlayerMakesTurn(playerId, card) =>
            state.modify(oldState => {
              val players     = oldState.players
              val scores      = players.map(_.score)
              val board       = Board.Empty
              val playerOpt   = players.find(_.id == playerId)
              val gameState   = for {
                player        <- playerOpt.toRight(WrongPlayerId(s"Player with provided playerId: $playerId not found in game state list of players"))
                _             <- if (isCardValidToMakeTurn(card, player, scores)) Right(card)
                                 else Left(WrongCard("Wrong card to make turn with received"))
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

              resolveRequest(oldState, gameState, playerId)
            })

          case PlayerMakesAttack(playerId, card) =>
            state.modify(oldState => {
              val players   = oldState.players
              val board     = oldState.board
              val playerOpt = players.find(_.id == playerId)
              val gameState = for {
                player                <- playerOpt.toRight(WrongPlayerId(s"Player with provided playerId: $playerId not found in game state list of players"))
                others                =  players.diff(List(player))
                strongestCard         <- board.strongestCard.toRight(UnexpectedGameState("Strongest card not found"))
                cardToHit             <- board.cardToHit.toRight(UnexpectedGameState("Card to hit not found"))
                _                     <- if (isCardValidToMakeAttack(card, player, strongestCard, cardToHit)) Right(card)
                                         else Left(WrongCard("Wrong card to make attack with received"))
                updatedPlayer         =  player.copy(hand = player.hand.removeCard(card))
                updatedPlayers        =  others :+ updatedPlayer
                newStrongestCard      =  if (isCardStronger(card, strongestCard.value)) StrongestCard(card, playerId)
                                         else strongestCard
                newBoard              =  board.addCard(card).copy(strongestCard = Some(newStrongestCard))
                winner                <- updatedPlayers.find(_.id == newStrongestCard.ownerId)
                                           .toRight(WrongPlayerId(s"Winner with provided playerId: ${newStrongestCard.ownerId} not found"))
                winnerWithAddedScore  =  winner.copy(score = Score(winner.score.value -1))
                winnerWithAddedTricks =  winnerWithAddedScore.copy(tricks = winnerWithAddedScore.tricks :+ Trick(newBoard.cards))
                newOthers             =  updatedPlayers.diff(List(winner))
                newPlayers            =  newOthers :+ winnerWithAddedTricks

              } yield GameState(
                deck        = oldState.deck,
                board       = newBoard,
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = Some(winnerWithAddedTricks),
                players     = newPlayers
              )

              resolveRequest(oldState, gameState, playerId)
            })

          case StartRound(playerId, deck) =>
            state.modify(oldState => {
              val players   = oldState.players
              val hands     = players.map(_ => Hand.Empty)
              val trumpCard = deck.cards.lastOption
              val gameState =
                for {
                  eitherOfDeckAndHands  <- deck.deal(hands).toRight(DealingError("There are no hands to deal cards to."))
                  (deck, dealtHands)    =  eitherOfDeckAndHands
                  whoseTurn             =  players.headOption
                  playersWithDealtHands =  players zip dealtHands map {
                    case (player, newHand) => player.addCardsToHand(newHand.cards)
                  }
                } yield GameState(
                  deck      = deck,
                  players   = playersWithDealtHands,
                  trumpCard = trumpCard,
                  whoseTurn = whoseTurn
                )

              resolveRequest(oldState, gameState, playerId)
            })

          case ResolveRound =>
            state.modify(oldState => {
              val players = oldState.players.map {
                player =>
                if (player.tricks.isEmpty) player.copy(score = Score(player.score.value + 5))
                else player
              }
              val gameState = GameState(
                deck        = oldState.deck,
                board       = Board.Empty,
                discardPile = oldState.discardPile,
                trumpCard   = oldState.trumpCard,
                whoseTurn   = None,
                players     = players.map(_.copy(tricks = List.empty))
              )

              val scores = players.map(player => s"${player.name}: ${player.score.value}")

              (gameState, EndRound(gameState.board, scores))
            })
        }


      private def resolveRequest(
                                  oldState: GameState,
                                  newState: Either[GameError, GameState],
                                  playerId: Long
                                ): (GameState, Response) =
        newState match {
          case Left(gameError) => (oldState, Error(gameError))
          case Right(newState) => newState.players.find(_.id == playerId) match {
            case None         =>
              (oldState, Error(WrongPlayerId(s"Player with provided playerId: $playerId not found in modified game state list of players")))
            case Some(player) =>
              val isGameOver      = newState.players.map(_.score.value).exists(_ <= 0)
              val playerCardsLeft = newState.players.map(_.cards.size)
              val isEmptyHands    = playerCardsLeft.count(_ == 0) == newState.players.size
              val gameScores      = newState.players.map(player => s"${player.name}: ${player.score.value}")
              val continueRound   =
                (newState, ContinueRound(newState.board, player.hand, newState.trumpCard, gameScores))
              val gameOver        =
                (newState, GameOver(newState.board, gameScores))
              val endRound        =
                (newState, EndRound(newState.board, gameScores))
              if (isGameOver) gameOver
              else if (isEmptyHands) endRound
              else continueRound
          }
        }
    }
}