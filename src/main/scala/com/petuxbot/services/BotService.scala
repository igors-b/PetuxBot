package com.petuxbot.services

import canoe.api._
import canoe.models.Chat
import canoe.models.messages.TextMessage
import canoe.syntax._
import cats.Applicative
import com.petuxbot.BotData.BotId
import com.petuxbot.Request._
import com.petuxbot.game.GameError.{ParsingError, WrongRequest}
import com.petuxbot.Codecs._
import com.petuxbot.Parser
import com.petuxbot.Command._
import com.petuxbot.Response._
import com.petuxbot.domain.cardcontainers._
import com.petuxbot.domain.{Player, Score}
import io.circe.syntax._
import cats.syntax.all._

trait BotService[F[_]] {
  def greetings: Scenario[F, Unit]
}

object BotService {

  def apply[F[_]: TelegramClient: Applicative](gameService: GameService[F], createDeck: DeckService[F]): BotService[F] =
    new BotService[F] {
      def greetings: Scenario[F, Unit] =
        for {
          chat          <- Scenario.expect(command("start").chat)
          detailedChat  <- Scenario.eval(chat.details)
          playerId      =  detailedChat.id
          userFirstName =  detailedChat.firstName.getOrElse("dear Friend")
          _             <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
          player        =  Player(playerId, userFirstName, Hand.Empty, Score(15), List.empty[Trick])
          dealer        =  Player(BotId, "Bot", Hand.Empty, Score(15), List.empty[Trick])
          response      <- Scenario.eval(gameService.process(AddPlayers(List(player, dealer))))
          _             <- response match {
            case OK => start(chat)
            case _  => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def showStatsToPlayer(chat: Chat, showBoardAndHandToPlayer: ShowBoardAndHandToPlayer): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Board is:")) >>
          Scenario.eval(chat.send(showBoardAndHandToPlayer.board.asJson.spaces2)) >>
          Scenario.eval(chat.send(s"Your cards:")) >>
          Scenario.eval(chat.send(showBoardAndHandToPlayer.hand.asJson.spaces2)) >>
          Scenario.eval(chat.send("Trump card is:")) >>
          Scenario.eval(chat.send(showBoardAndHandToPlayer.trumpCard.asJson.spaces2)) >>
          Scenario.eval(chat.send("Game score:")) >>
          Scenario.eval(chat.send(showBoardAndHandToPlayer.scores.asJson.spaces2))
      }

      private def showTotalsToPlayer(chat: Chat, showTotalsToPlayer: ShowTotalsToPlayer): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Board is:")) >>
          Scenario.eval(chat.send(showTotalsToPlayer.board.asJson.spaces2)) >>
          Scenario.eval(chat.send("Game score:")) >>
          Scenario.eval(chat.send(showTotalsToPlayer.scores.asJson.spaces2))
      }

      private def start(chat: Chat): Scenario[F, Unit] =
        for {
          _              <- Scenario.eval(chat.send("Type Deal to start new round:"))
          detailedChat   <- Scenario.eval(chat.details)
          playerId       =  detailedChat.id
          requestString  <- Scenario.expect(text)
          deck           <- Scenario.eval(createDeck.of)
          response       <- Parser.parse(requestString) match {
            case Right(request)  => request match {
              case StartNewRound   => Scenario.eval(gameService.process(StartRound(playerId, deck)))
              case _               => Scenario.eval(Error(WrongRequest).pure)
            }
            case Left(error) => Scenario.eval(Error(ParsingError(error.getMessage)).pure)
          }

          _ <- response match {

            case s: ShowBoardAndHandToPlayer =>
              Scenario.eval(chat.send(s"Round started")) >>
                showStatsToPlayer(chat, s) >>
                changeCards(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                start(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def changeCards(chat: Chat): Scenario[F, Unit] =
        for {
          _              <-  Scenario.eval(chat.send(s"Would you like to change some cards?"))
          _              <-  Scenario.eval(chat.send(s"Send me the list of cards you would like to change"))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-   Parser.parse(requestString) match {
            case Right(request)  => request match {
              case ChangeCards(cards) => Scenario.eval(gameService.process(ChangeCardsForPlayer(playerId, cards)))
              case _                  => Scenario.eval(Error(WrongRequest).pure)
            }
            case Left(error) => Scenario.eval(Error(ParsingError(error.getMessage)).pure)
          }
          _ <- response match {

            case s: ShowBoardAndHandToPlayer =>
              showStatsToPlayer(chat, s) >>
                defineWhoseTurn(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                changeCards(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def defineWhoseTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send(s"Requesting playerID whose turn"))
          detailedChat <- Scenario.eval(chat.details)
          id           =  detailedChat.id
          response     <- Scenario.eval(gameService.process(GetPlayerIdWhoseTurn))
          _            <- response match {
            case WhoseTurn(playerId) =>
              if (playerId == id) playerMakesTurn(chat)
              else botMakesTurn(chat)
            case Error(_) => Scenario.eval(chat.send(response.asJson.spaces2))
            case _        => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def playerMakesTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _              <-  Scenario.eval(chat.send("Now it is Your turn. Put card on the Board"))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-  Parser.parse(requestString) match {
            case Right(request) => request match {
              case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesTurn(playerId, card)))
              case _                      => Scenario.eval(Error(WrongRequest).pure)
            }
            case Left(error) => Scenario.eval(Error(ParsingError(error.getMessage)).pure)
          }
          _            <- response match {

            case s: ShowBoardAndHandToPlayer =>
              showStatsToPlayer(chat, s) >>
                botMakesAttack(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def playerMakesAttack(chat: Chat): Scenario[F, Unit] =
        for {
          _              <-  Scenario.eval(chat.send("Now it's time to ATTACK!"))
          _              <-  Scenario.eval(chat.send("Put card on the Board:"))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-  Parser.parse(requestString) match {
            case Right(request) => request match {
              case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesAttack(playerId, card)))
              case _                      => Scenario.eval(Error(WrongRequest).pure)
            }
            case Left(error) => Scenario.eval(Error(ParsingError(error.getMessage)).pure)
          }
          _            <- response match {

            case s: ShowBoardAndHandToPlayer =>
              showStatsToPlayer(chat, s) >>
                defineWhoseTurn(chat)

            case s: ShowTotalsToPlayer =>
              showTotalsToPlayer(chat, s)
                resolveRound(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesAttack(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def botMakesAttack(chat: Chat): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Bot makes attack"))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesAttack(playerId)))
          _            <- response match {

            case s: ShowBoardAndHandToPlayer =>
              showStatsToPlayer(chat, s) >>
                defineWhoseTurn(chat)

            case s: ShowTotalsToPlayer =>
              showTotalsToPlayer(chat, s) >>
                resolveRound(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                botMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def botMakesTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Now it is Bot's turn."))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesTurn(playerId)))
          _            <- response match {

            case s: ShowBoardAndHandToPlayer =>
              showStatsToPlayer(chat, s) >>
                playerMakesAttack(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                botMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def resolveRound(chat: Chat): Scenario[F, Unit] =
        for {
          _         <- Scenario.eval(chat.send("Round ended!"))
          response  <- Scenario.eval(gameService.process(ResolveRound))
          _         <- response match {
            case ShowTotalsToPlayer(_, scores) =>
              Scenario.eval(chat.send("Game score after round resolvement is :")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                start(chat)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()
    }
}
