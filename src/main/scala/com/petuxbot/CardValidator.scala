package com.petuxbot

import com.petuxbot.domain.{Card, Player, Score}

object CardValidator {
  def isCardValidToMakeTurn(card: Card, player: Player, scores: List[Score]): Boolean =
    if (player.hand.cards.contains(card))
      scores.exists(_.value <=3) && card.isTrump || !scores.exists(_.value <=3)
    else false

}
