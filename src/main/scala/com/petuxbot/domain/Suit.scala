package com.petuxbot.domain

import cats.data.NonEmptyList

sealed trait Suit

object Suit {

  lazy val Suits = NonEmptyList(Clubs, List(Diamonds, Hearts, Spades))

  case object Clubs extends Suit {
    override def toString: String = "c"
  }

  case object Diamonds extends Suit {
    override def toString: String = "d"
  }

  case object Hearts extends Suit {
    override def toString: String = "h"
  }

  case object Spades extends Suit {
    override def toString: String = "s"
  }
}
