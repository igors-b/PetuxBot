package com.petuxbot
import com.petuxbot.domain.Card
import io.circe.generic.JsonCodec
sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  case class ShowCardsToPlayer(cards: List[Card], trumpCard: Card) extends Response
  case class Error(errorDescription: String) extends Response
}
