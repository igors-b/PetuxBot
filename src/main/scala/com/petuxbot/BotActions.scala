package com.petuxbot

import canoe.api._
import canoe.syntax._
import canoe.models.Chat
import canoe.syntax.{command, text}
import cats.effect.Sync
import com.petuxbot.Response.{Error, OK}
import com.petuxbot.domain.{GameState, Hand, Player, Score, Trick}

object BotActions {
  def greetings[F[_] : TelegramClient : Sync]: Scenario[F, Unit] =
    for {
      chat <- Scenario.expect(command("hello").chat)
      detailedChat <- Scenario.eval(chat.details)
      userFirstName = detailedChat.firstName.getOrElse("dear Friend")
      _ <- Scenario.eval(chat.send(s"Hello, $userFirstName! Would you like to start PETUX game?"))
      player = Player(userFirstName, Hand.empty, Score(15), Vector.empty[Trick])
      dealer = Player("Bot", Hand.empty, Score(15), Vector.empty[Trick])
      _ <- start(chat, Vector(player, dealer))
    } yield ()

  def start[F[_] : TelegramClient : Sync](chat: Chat, players: Vector[Player]): Scenario[F, Unit] =
    for {
      _          <- Scenario.eval(chat.send("Start game by typing START"))
      resp       <- Scenario.expect(text)
      cmd        <- Scenario.eval(Parser.parse(resp))
      gameState  <- Scenario.eval(GameState.make(players))
      result     <- Scenario.eval(CommandProcessor.process(gameState, cmd))
      (state, response) = result
      _ <- response match {
        case OK => startGame(chat, state)
        case Error(errorDescription) => Scenario.eval(chat.send(s"$errorDescription")) >>
          start(chat, players)
      }
    } yield ()

  def startGame[F[_] : TelegramClient: Sync](chat: Chat, state: GameState[F]): Scenario[F, Unit] = {
    val p = state.players.head.hand.cards.head.suit.toString
    for {
      _ <- Scenario.eval(chat.send(s"Game started $p"))
    } yield ()
  }
}
