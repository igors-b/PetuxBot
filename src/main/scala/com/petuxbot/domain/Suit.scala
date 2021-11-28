package com.petuxbot.domain

import cats.data.NonEmptyList
import enumeratum._

sealed trait Suit extends EnumEntry

object Suit extends Enum[Suit]{

  override def values: IndexedSeq[Suit] = findValues

  //lazy val Suits = NonEmptyList(Clubs, List(Diamonds, Hearts, Spades))

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
