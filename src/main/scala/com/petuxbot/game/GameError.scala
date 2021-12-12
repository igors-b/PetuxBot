package com.petuxbot.game

sealed trait GameError

object GameError {
  case object ParsingError extends GameError
  case object WrongCommand extends GameError
  case object WrongPlayerId extends GameError
  case object WrongCard extends GameError
  case object WhoseTurnNotSet extends GameError

}


