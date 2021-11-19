package com.petuxbot.domain

import com.petuxbot.domain.Rank.{Ace, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}

case class Card(rank: Rank, suit: Suit, isTrump: Boolean = false) {

  def strength: Int =
    rank match {
      case Ace   => 14
      case King  => 13
      case Queen => 12
      case Jack  => 11
      case Ten   => 10
      case Nine  => 9
      case Eight => 8
      case Seven => 7
      case Six   => 6
      case Five  => 5
      case Four  => 4
      case Three => 3
      case Two   => 2
    }

  override def toString: String = rank.toString + suit.toString
}
