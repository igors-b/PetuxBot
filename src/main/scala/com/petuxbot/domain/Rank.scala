package com.petuxbot.domain

import enumeratum._

sealed trait Rank extends EnumEntry

object Rank extends Enum[Rank] {

  override def values: IndexedSeq[Rank] = findValues

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
  case object Nine extends Rank {
    override def toString: String = "9"
  }
  case object Eight extends Rank {
    override def toString: String = "8"
  }
  case object Seven extends Rank {
    override def toString: String = "7"
  }
  case object Six extends Rank {
    override def toString: String = "6"
  }
  case object Five extends Rank {
    override def toString: String = "5"
  }
  case object Four extends Rank {
    override def toString: String = "4"
  }
  case object Three extends Rank {
    override def toString: String = "3"
  }
  case object Two extends Rank {
    override def toString: String = "2"
  }

//  def make(rank: String): Rank = {
//    rank match {
//      case "a" => Ace
//      case "k" => King
//      case "q" => Queen
//      case "j" => Jack
//      case "t" => Ten
//      case "9" => Nine
//      case "8" => Eight
//      case "7" => Seven
//      case "6" => Six
//      case "5" => Five
//      case "4" => Four
//      case "3" => Three
//      case "2" => Two
//      //case _ => Left("Error invalid rank")
//    }
//  }
}

