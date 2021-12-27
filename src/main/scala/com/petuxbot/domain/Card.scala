package com.petuxbot.domain

import com.petuxbot.domain.Rank._

final case class Card(rank: Rank, suit: Suit, isTrump: Boolean = false) {

  def strength: Int =
    rank match {
      case Ace   => 14
      case King  => 13
      case Queen => 12
      case Jack  => 11
      case Ten   => 10
    }
}
