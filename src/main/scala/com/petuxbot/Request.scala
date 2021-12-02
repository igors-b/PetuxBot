package com.petuxbot

import com.petuxbot.domain.{Card, Player}
import com.petuxbot.domain.cardContainers.Deck

sealed trait Request
object Request {
  final case class AddPlayers(players: List[Player]) extends Request
  final case class ChangeCardsForPlayer(playerId: Long, cards: List[Card]) extends Request
  case object GetPlayerIdWhoseTurn extends Request
  final case class PlayerMakesTurn(playerId: Long, card: Card) extends Request
  final case class StartRound(playerId: Long, deck: Deck) extends Request

  case object WrongRequest extends Request
}



