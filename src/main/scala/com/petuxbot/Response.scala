package com.petuxbot
import com.petuxbot.domain.cardcontainers.{Board, Hand}
import com.petuxbot.domain.Card
sealed trait Response

object Response {
  type ErrorDescription = String
  case object OK extends Response
  final case class ShowBoardAndHandToPlayer(board: Board, hand: Hand, trumpCard: Option[Card], scores: List[String]) extends Response
  final case class ShowTotalsToPlayer(board: Board, scores: List[String]) extends Response
  final case class WhoseTurn(playerId: Long) extends Response
  final case class Error(error: GameError) extends Response
}
