package com.petuxbot

import com.petuxbot.domain._

sealed trait Command

object Command {
  case object StartNewRound extends Command
  final case class ChangeCards(cards: List[Card]) extends Command
  final case class MakeTurnWithCard(card: Card) extends Command
}
