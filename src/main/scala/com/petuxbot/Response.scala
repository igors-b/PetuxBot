package com.petuxbot
import com.petuxbot.domain.cardcontainers.{Board, Hand}
import com.petuxbot.domain.Card
import com.petuxbot.game.GameError
sealed trait Response

object Response {
  case object OK extends Response
  final case class GameStateData(board: Board, hand: Hand, trumpCard: Option[Card], scores: List[String]) extends Response
  final case class GameOver(board: Board, scores: List[String]) extends Response
  final case class Totals(board: Board, scores: List[String]) extends Response
  final case class WhoseTurn(playerId: Long) extends Response
  final case class Error(error: GameError) extends Response
}
