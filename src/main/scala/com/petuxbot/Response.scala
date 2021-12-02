package com.petuxbot
import com.petuxbot.domain.Card
sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  final case class ShowCardsToPlayer(cards: List[Card], trumpCard: Option[Card]) extends Response
  final case class Error(errorDescription: String) extends Response
}
