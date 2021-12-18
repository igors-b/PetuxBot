package com.petuxbot.domain.cardcontainers

import com.petuxbot.domain.Card

final case class DiscardPile (cards: List[Card]) {
  def addCards(cardsToAdd: List[Card]): DiscardPile = copy(cards ++ cardsToAdd)
}

object DiscardPile {
  lazy val Empty: DiscardPile = DiscardPile(List.empty[Card])
}
