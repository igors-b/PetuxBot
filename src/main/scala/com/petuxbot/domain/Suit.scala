package com.petuxbot.domain

import enumeratum._

sealed trait Suit extends EnumEntry

object Suit extends Enum[Suit]{

  override def values: IndexedSeq[Suit] = findValues

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

//  def make(suit: String): Suit =
//    suit match {
//      case "c" => Clubs
//      case "d" => Diamonds
//      case "h" => Hearts
//      case "s" => Spades
//    }

}
