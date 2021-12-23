package domain.cardcontainers

import com.petuxbot.domain.{Card, StrongestCard}
import com.petuxbot.domain.Rank.King
import com.petuxbot.domain.Suit.Diamonds
import com.petuxbot.domain.cardcontainers.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardSpec extends AnyFlatSpec with Matchers{

  val board: Board = Board.Empty
  val card: Card = Card(King, Diamonds)
  val playerId = 0

  "addCard" should "return new board with added card" in {
    board.cards.contains(card) shouldEqual false
    board.addCard(card).cards.contains(card) shouldEqual true
  }

  "setCardToHit" should "return new board with cardToHit set" in {
    board.cardToHit shouldEqual None
    board.setCardToHit(card).cardToHit shouldEqual Some(card)
  }

  "setStrongestCard" should "return new board with strongestCard set" in {
    board.strongestCard shouldEqual None
    board.setStrongestCard(card, playerId).strongestCard shouldEqual Some(StrongestCard(card, playerId))
  }
}
