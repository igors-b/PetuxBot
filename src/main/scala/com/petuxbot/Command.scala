package com.petuxbot

import com.petuxbot.domain.{Card, Player}
import com.petuxbot.domain.cardcontainers.Deck

sealed trait Command
object Command {
  final case class AddPlayers(players: List[Player]) extends Command
  final case class BotMakesAttack(playerId: Long) extends Command
  final case class BotMakesTurn(playerId: Long) extends Command
  final case class ChangeCardsForPlayer(playerId: Long, cards: List[Card]) extends Command
  case object GetPlayerIdWhoseTurn extends Command
  final case class PlayerMakesTurn(playerId: Long, card: Card) extends Command
  final case class PlayerMakesAttack(playerId: Long, card: Card) extends Command
  case object ResolveRound extends Command
  final case class StartRound(playerId: Long, deck: Deck) extends Command
}



