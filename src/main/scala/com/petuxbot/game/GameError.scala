package com.petuxbot.game

sealed trait GameError

object GameError {

  final case class CardValidationError(errorDescription: String) extends GameError
  final case class DealingError(errorDescription: String) extends GameError
  final case class ParsingError(errorDescription: String) extends GameError
  final case class UnexpectedGameState(errorDescription: String) extends GameError
  case object WrongRequest extends GameError
  final case class WrongPlayerId(errorDescription: String) extends GameError
  final case class WrongCard(errorDescription: String) extends GameError
  final case class WrongCardsToChange(errorDescription: String) extends GameError

}


