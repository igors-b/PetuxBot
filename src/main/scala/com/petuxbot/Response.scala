package com.petuxbot
import com.petuxbot.domain.cardContainers.{Board, Hand}
import com.petuxbot.domain.{Card, Player}
sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  final case class ShowBoardAndHandToPlayer(board: Board, hand: Hand, trumpCard: Option[Card]) extends Response
  final case class WhoseTurn(playerId: Long) extends Response
  final case class Error(errorDescription: String) extends Response
}
