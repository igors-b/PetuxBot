package com.petuxbot

import com.petuxbot.domain._

sealed trait Request

object Request {
  case object StartNewRound extends Request
  final case class ChangeCards(cards: List[Card]) extends Request
  final case class MakeTurnWithCard(card: Card) extends Request
}
