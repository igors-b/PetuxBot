package com.petuxbot.domain.cardcontainers

import cats.implicits._
import com.petuxbot.domain.Card

final case class Deck(cards: List[Card]) {

  def deal(hands: List[Hand]): Option[(Deck, List[Hand])] = {
    if (hands.nonEmpty) {
      if (cards.nonEmpty) {
        hands.foldLeft((List.empty[Hand], cards)) {
          case ((hands, cards), hand) =>
            val (taken, rest) = cards.splitAt(Hand.InitialNumberOfCards - hand.cards.size)
            (hands :+ hand.addCards(taken), rest)
        } match {
          case (hands, cards) => (copy(cards = cards), hands).some
        }
      } else (this, hands).some
    } else None
  }
}

object Deck {
  lazy val Empty: Deck = Deck(List.empty)
}
