package com.petuxbot.domain

import cats.data.NonEmptyList


sealed trait Rank

object Rank {

  lazy val Ranks: NonEmptyList[Rank] = NonEmptyList(Ace, List(King, Queen, Jack, Ten))

  case object Ace extends Rank {
    override def toString: String = "A"
  }
  case object King extends Rank {
    override def toString: String = "K"
  }
  case object Queen extends Rank {
    override def toString: String = "Q"
  }
  case object Jack extends Rank {
    override def toString: String = "J"
  }
  case object Ten extends Rank {
    override def toString: String = "T"
  }
}

