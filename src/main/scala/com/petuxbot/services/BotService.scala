package com.petuxbot.services

import canoe.api._
import canoe.models.Chat
import canoe.models.messages.TextMessage
import canoe.syntax._
import cats.effect.Sync
import com.petuxbot.BotData.BotId
import com.petuxbot.Request._
import com.petuxbot.game.GameError.{ParsingError, WrongRequest}
import com.petuxbot.Codecs._
import com.petuxbot.{Parser, Request, Response, domain}
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

  def apply[F[_]: TelegramClient: Sync](createDeck: DeckService[F]): BotService[F] =
    new BotService[F] {
      def greetings: Scenario[F, Unit] =
        for {
          chat          <- Scenario.expect(command("start").chat)
          detailedChat  <- Scenario.eval(chat.details)
          gameService   <- Scenario.eval(GameService.of)
          playerId      =  detailedChat.id
          userFirstName =  detailedChat.firstName.getOrElse("dear Friend")
          _             <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
          player        =  domain.Player(playerId, userFirstName, Hand.Empty, Score(15), List.empty[Trick])
          dealer        =  Player(BotId, "Bot", Hand.Empty, Score(15), List.empty[Trick])
          response      <- Scenario.eval(gameService.process(AddPlayers(List(player, dealer))))
          _             <- response match {
            case OK => start(chat, gameService)
            case _  => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def start(chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
        val keyboard = KeyboardService.create("\"deal\"")
        for {
          _              <- Scenario.eval(chat.send("Type \"deal\" to start new round:", keyboard = keyboard))
          detailedChat   <- Scenario.eval(chat.details)
          playerId       =  detailedChat.id
          requestString  <- Scenario.expect(text)
          deck           <- Scenario.eval(createDeck.of)
          response       <- getResponse(requestString) {
            case StartNewRound => Scenario.eval(gameService.process(StartRound(playerId, deck)))
            case _             => Scenario.raiseError(new Exception(""))
          }
          _ <- response match {

            case gameData: ContinueRound =>
              Scenario.eval(chat.send(s"Round started")) >>
                showCardsToPlayer(chat, gameData) >>
                changeCards(chat, gameService)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                start(chat, gameService)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()
      }


      private def changeCards(chat: Chat, gameService: GameService[F]): Scenario[F, Unit] = {
        val keyboard = KeyboardService.create("{\"cards\":[]}")
        for {
          _              <-  Scenario.eval(chat.send(s"Would you like to change some cards?"))
          _              <-  Scenario.eval(chat.send(s"Send me the list of cards you would like to change",
                                           keyboard = keyboard))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-  getResponse(requestString) {
            case ChangeCards(cards) => Scenario.eval(gameService.process(ChangeCardsForPlayer(playerId, cards)))
            case _                  => Scenario.raiseError(new Exception(""))
          }
          _ <- response match {

            case continueRound: ContinueRound =>
                defineWhoseTurn(chat, gameService, continueRound)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                changeCards(chat, gameService)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()
      }


      private def defineWhoseTurn(chat: Chat, gameService: GameService[F], continueRound: ContinueRound): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send(s"Requesting playerID whose turn"))
          detailedChat <- Scenario.eval(chat.details)
          id           =  detailedChat.id
          response     <- Scenario.eval(gameService.process(GetPlayerIdWhoseTurn))
          _            <- response match {
            case WhoseTurn(playerId) =>
              if (playerId == id) playerMakesTurn(chat, gameService, continueRound)
              else botMakesTurn(chat, gameService)
            case Error(_) => Scenario.eval(chat.send(response.asJson.spaces2))
            case _        => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def playerMakesTurn(chat: Chat, gameService: GameService[F], continueRound: ContinueRound): Scenario[F, Unit] = {
        val buttons = continueRound.hand.cards.map(card => MakeTurnWithCard(card).asJson.noSpaces)
        val keyboard = KeyboardService.createFromList(buttons)
        for {
          _              <-  Scenario.eval(chat.send("Now it is Your turn. Put card on the Board",
                                           keyboard = keyboard
                                           ))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-  getResponse(requestString) {
            case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesTurn(playerId, card)))
            case _                      => Scenario.raiseError(new Exception(""))
          }
          _  <- response match {

            case continueRound: ContinueRound =>
              showBoardToPlayer(chat, continueRound) >>
                botMakesAttack(chat, gameService)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesTurn(chat, gameService, continueRound)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()
      }

      private def playerMakesAttack(chat: Chat, gameService: GameService[F], gameStateData: ContinueRound): Scenario[F, Unit] = {
        val buttons = gameStateData.hand.cards.map(card => MakeTurnWithCard(card).asJson.noSpaces)
        val keyboard = KeyboardService.createFromList(buttons)
        for {
          _              <-  Scenario.eval(chat.send("Now it's time to ATTACK!"))
          _              <-  Scenario.eval(chat.send("Put card on the Board:", keyboard = keyboard))
          detailedChat   <-  Scenario.eval(chat.details)
          playerId       =   detailedChat.id
          requestString  <-  Scenario.expect(text)
          response       <-  getResponse(requestString) {
            case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesAttack(playerId, card)))
            case _                      => Scenario.raiseError(new Exception(""))
          }

          _ <- response match {

            case continueRound: ContinueRound =>
              showBoardAndScoreToPlayer(chat, continueRound) >>
                defineWhoseTurn(chat, gameService, continueRound)

            case endRound: EndRound =>
              showTotalsToPlayer(chat, endRound) >>
                resolveRound(chat, gameService)

            case gameOver: GameOver => showGameResult(chat, gameOver)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesAttack(chat, gameService, gameStateData)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()
      }


      private def botMakesAttack(chat: Chat, gameService: GameService[F]): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Bot makes attack"))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesAttack(playerId)))
          _            <- response match {

            case continueRound: ContinueRound =>
              showBoardAndScoreToPlayer(chat, continueRound) >>
                defineWhoseTurn(chat, gameService, continueRound)

            case endRound: EndRound =>
              showTotalsToPlayer(chat, endRound) >>
                resolveRound(chat, gameService)

            case gameOver: GameOver => showGameResult(chat, gameOver)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                botMakesTurn(chat, gameService)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def botMakesTurn(chat: Chat, gameService: GameService[F]): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Now it is Bot's turn."))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesTurn(playerId)))
          _            <- response match {

            case continueRound: ContinueRound =>
              showBoardAndTrumpCardToPlayer(chat, continueRound) >>
                playerMakesAttack(chat, gameService, continueRound)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                botMakesTurn(chat, gameService)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()

      private def resolveRound(chat: Chat, gameService: GameService[F]): Scenario[F, Unit] =
        for {
          _         <- Scenario.eval(chat.send("Round ended!"))
          response  <- Scenario.eval(gameService.process(ResolveRound))
          _         <- response match {
            case EndRound(_, scores) =>
              Scenario.eval(chat.send("Game score after round resolvement is :")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                start(chat, gameService)
            case _ => Scenario.eval(chat.send("Wrong response from server"))
          }
        } yield ()


      private def getResponse(requestString: String)(pf: PartialFunction[Request, Scenario[F, Response]]): Scenario[F, Response] =
        Parser.parse(requestString).fold(
          error   => Scenario.pure(Error(ParsingError(error.getMessage))),
          request => pf(request) orElse Scenario.pure(Error(WrongRequest))
        )


      private def showBoardToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Board is:")) >>
          Scenario.eval(chat.send(continueRound.board.asJson.spaces2))
      }

      private def showTrumpCardToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Trump card is:")) >>
          Scenario.eval(chat.send(continueRound.trumpCard.asJson.spaces2))
      }

      private def showScoreToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Game score:")) >>
          Scenario.eval(chat.send(continueRound.scores.asJson.spaces2))
      }

      private def showBoardAndTrumpCardToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        showBoardToPlayer(chat, continueRound)  >>
          showTrumpCardToPlayer(chat, continueRound)
      }

      private def showBoardAndScoreToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        showBoardToPlayer(chat, continueRound)  >>
          showScoreToPlayer(chat, continueRound)
      }

      private def showCardsToPlayer(chat: Chat, continueRound: ContinueRound): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send(s"Your cards:")) >>
          Scenario.eval(chat.send(continueRound.hand.asJson.spaces2)) >>
          showTrumpCardToPlayer(chat, continueRound) >>
          showScoreToPlayer(chat, continueRound)
      }

      private def showTotalsToPlayer(chat: Chat, endRound: EndRound): Scenario[F, TextMessage] = {
        Scenario.eval(chat.send("Board is:")) >>
          Scenario.eval(chat.send(endRound.board.asJson.spaces2)) >>
          Scenario.eval(chat.send("Game score:")) >>
          Scenario.eval(chat.send(endRound.scores.asJson.spaces2))
      }

      private def showGameResult(chat: Chat, gameOver: GameOver): Scenario[F, TextMessage] = {
        val keyboard = KeyboardService.create("/start")
        Scenario.eval(chat.send("Board is:")) >>
          Scenario.eval(chat.send(gameOver.board.asJson.spaces2)) >>
          Scenario.eval(chat.send("Game score:")) >>
          Scenario.eval(chat.send(gameOver.scores.asJson.spaces2)) >>
          Scenario.eval(chat.send("GAME IS OVER!", keyboard = keyboard))
      }
    }
}
