package com.petuxbot.domain

import cats.data.NonEmptyList


sealed trait Rank

object Rank {

  lazy val Ranks = NonEmptyList(Ace, List(King, Queen, Jack, Ten))

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

//  case object Nine extends Rank {
//    override def toString: String = "9"
//  }
//  case object Eight extends Rank {
//    override def toString: String = "8"
//  }
//  case object Seven extends Rank {
//    override def toString: String = "7"
//  }
//  case object Six extends Rank {
//    override def toString: String = "6"
//  }
//  case object Five extends Rank {
//    override def toString: String = "5"
//  }
//  case object Four extends Rank {
//    override def toString: String = "4"
//  }
//  case object Three extends Rank {
//    override def toString: String = "3"
//  }
//  case object Two extends Rank {
//    override def toString: String = "2"
//  }
}

