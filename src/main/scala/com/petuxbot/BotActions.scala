package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import com.petuxbot.Command._
import com.petuxbot.Response._
import com.petuxbot.Request._
import com.petuxbot.ImplicitCodecs._
import com.petuxbot.Request.AddPlayers
import com.petuxbot.domain.cardContainers._
import com.petuxbot.domain.{Player, Score}
import com.petuxbot.services.{CreateDeck, GameService}
import io.circe.syntax._

trait BotActions[F[_]] {
  def greetings: Scenario[F, Unit]
  def start(chat: Chat): Scenario[F, Unit]
  def changeCards(chat: Chat): Scenario[F, Unit]
  def defineWhoseTurn(chat: Chat): Scenario[F, Unit]
  def playerMakesTurn(chat: Chat): Scenario[F, Unit]
  def playerMakesAttack(chat: Chat): Scenario[F, Unit]
  def botMakesAttack(chat: Chat): Scenario[F, Unit]
  def botMakesTurn(chat: Chat): Scenario[F, Unit]
  def resolveRound(chat: Chat): Scenario[F, Unit]
}

object BotActions {

  def apply[F[_]: TelegramClient](gameService: GameService[F], createDeck: CreateDeck[F]): BotActions[F] =
    new BotActions[F] {
      def greetings: Scenario[F, Unit] =
        for {
          chat          <- Scenario.expect(command("StartGame").chat)
          detailedChat  <- Scenario.eval(chat.details)
          id            =  detailedChat.id
          userFirstName =  detailedChat.firstName.getOrElse("dear Friend")
          _             <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
          player        =  Player(id, userFirstName, Hand.Empty, Score(15), List.empty[Trick])
          dealer        =  Player(0, "Bot", Hand.Empty, Score(15), List.empty[Trick])
          _             <- Scenario.eval(gameService.process(AddPlayers(List(player, dealer))))
          _             <- start(chat)
        } yield ()

      def start(chat: Chat): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Type Deal to start new round Start game by typing StartGame"))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          resp         <- Scenario.expect(text)
          cmd          =  Parser.parse(resp)
          deck         <- Scenario.eval(createDeck.apply())
          response <- cmd match {
            case StartNewRound => Scenario.eval(gameService.process(StartRound(playerId, deck)))
            case _             => ??? //Scenario.eval(gameService.process(WrongCommand))
          }
          _ <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, scores) =>
              Scenario.eval(chat.send(s"Game started, your cards:")) >>
                Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                Scenario.eval(chat.send("Trump card is:")) >>
                Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                changeCards(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                start(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()

      def changeCards(chat: Chat): Scenario[F, Unit] =
        for {
          _            <-  Scenario.eval(chat.send(s"Would you like to change some cards?"))
          _            <-  Scenario.eval(chat.send(s"Send me the list of cards you would like to change"))
          detailedChat <-  Scenario.eval(chat.details)
          playerId     =   detailedChat.id
          resp         <-  Scenario.expect(text)
          command      =   Parser.parse(resp)
          response     <-  command match {
            case ChangeCards(cards) => Scenario.eval(gameService.process(ChangeCardsForPlayer(playerId, cards)))
            case _                  => ??? //Scenario.eval(gameService.process(WrongCommand))
          }
          _            <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, scores) =>
              Scenario.eval(chat.send(s"Your cards:")) >>
                Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                Scenario.eval(chat.send("Trump card is:")) >>
                Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                defineWhoseTurn(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                changeCards(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()


      def defineWhoseTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _ <- Scenario.eval(chat.send(s"Requesting playerID whose turn"))
          detailedChat <- Scenario.eval(chat.details)
          id     =  detailedChat.id
          response <- Scenario.eval(gameService.process(GetPlayerIdWhoseTurn))
          _ <- response match {
            case WhoseTurn(playerId) =>
              if (playerId == id) playerMakesTurn(chat)
              else botMakesTurn(chat)
            case Error(_) => ???
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()


      def playerMakesTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _ <- Scenario.eval(chat.send("Now it is Your turn. Put card on the Board"))
          detailedChat <-  Scenario.eval(chat.details)
          playerId     =   detailedChat.id
          resp         <-  Scenario.expect(text)
          command      =   Parser.parse(resp)
          response     <-  command match {
            case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesTurn(playerId, card)))
            case _                      => ??? //Scenario.eval(gameService.process(WrongCommand))
          }
          _            <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, _) =>
              Scenario.eval(chat.send("Board is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                Scenario.eval(chat.send(s"Your cards:")) >>
                Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                Scenario.eval(chat.send("Trump card is:")) >>
                Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                botMakesAttack(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()

      def playerMakesAttack(chat: Chat): Scenario[F, Unit] =
        for {
          _            <-  Scenario.eval(chat.send("Now it's time to ATTACK!"))
          _            <-  Scenario.eval(chat.send("Put card on the Board:"))
          detailedChat <-  Scenario.eval(chat.details)
          playerId     =   detailedChat.id
          resp         <-  Scenario.expect(text)
          command      =   Parser.parse(resp)
          response     <-  command match {
            case MakeTurnWithCard(card) => Scenario.eval(gameService.process(PlayerMakesAttack(playerId, card)))
            case _                      => ??? //Scenario.eval(gameService.process(WrongCommand))
          }
          _            <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, scores) =>
              Scenario.eval(chat.send("Board after your attack is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                Scenario.eval(chat.send(s"Your cards:")) >>
                Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                Scenario.eval(chat.send("Trump card is:")) >>
                Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                defineWhoseTurn(chat)

            case ShowTotalsToPlayer(board, scores) =>
              Scenario.eval(chat.send("Board after your attack is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                resolveRound(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesAttack(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()


      def botMakesAttack(chat: Chat): Scenario[F, Unit] =
        for {
          _            <- Scenario.eval(chat.send("Bot makes attack"))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesAttack(playerId)))
          _            <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, scores) =>
              Scenario.eval(chat.send("Board after attack is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                Scenario.eval(chat.send(s"Your cards:")) >>
                Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                Scenario.eval(chat.send("Trump card is:")) >>
                Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                defineWhoseTurn(chat)

            case ShowTotalsToPlayer(board, scores) =>
              Scenario.eval(chat.send("Board after your attack is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                Scenario.eval(chat.send("Game score:")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                resolveRound(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()

      def botMakesTurn(chat: Chat): Scenario[F, Unit] =
        for {
          _ <- Scenario.eval(chat.send("Now it is Bot's turn."))
          detailedChat <- Scenario.eval(chat.details)
          playerId     =  detailedChat.id
          response     <- Scenario.eval(gameService.process(BotMakesTurn(playerId)))
          _            <- response match {

            case ShowBoardAndHandToPlayer(board, hand, trumpCard, scores) =>
              Scenario.eval(chat.send("Board after Bot's turn is:")) >>
                Scenario.eval(chat.send(board.asJson.spaces2)) >>
                //            Scenario.eval(chat.send(s"Your cards:")) >>
                //            Scenario.eval(chat.send(hand.asJson.spaces2)) >>
                //            Scenario.eval(chat.send("Trump card is:")) >>
                //            Scenario.eval(chat.send(trumpCard.asJson.spaces2)) >>
                //            Scenario.eval(chat.send("Game score:")) >>
                //            Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                playerMakesAttack(chat)

            case Error(_) =>
              Scenario.eval(chat.send(response.asJson.spaces2)) >>
                playerMakesTurn(chat)
            case _ => Scenario.eval(chat.send("Error appeared"))
          }
        } yield ()

      def resolveRound(chat: Chat): Scenario[F, Unit] =
        for {
          _         <- Scenario.eval(chat.send("Round ended!"))
          response  <- Scenario.eval(gameService.process(ResolveRound))
          _         <- response match {
            case ShowTotalsToPlayer(_, scores) =>
              Scenario.eval(chat.send("Game score after round resolvement is :")) >>
                Scenario.eval(chat.send(scores.asJson.spaces2)) >>
                start(chat)
          }
        } yield ()
    }
}
