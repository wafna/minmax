package wafna.games.onitama

import wafna.TestBase
import wafna.games.onitama.Deck._

class OnitamaTest extends TestBase {

  implicit class HandContainsCard(hand: Hand) {
    def contains(card: Card): Boolean = hand match {
      case Hand(`card`, _) => true
      case Hand(_, `card`) => true
      case _ => false
    }
  }

  "Onitama" should {
    "Calculate moves" in {
      val g0 = Onitama(Hand(Monkey, Dragon),Hand(Tiger, Ox), Horse)
      val moves = g0.moves()
      // 4 pieces can use monkey to go up left
      // 4 pieces can use monkey to go up right
      // 3 for dragon up left and 3 for dragon up right
      assertResult(14)(moves.size)
      // P2 has turn in hand.
      assert(moves.forall(_.pass.isLeft))
      // Each card in hand used as expected.
      assertResult(8)(moves.iterator.count(_.pass == Left(Monkey)))
      assertResult(6)(moves.iterator.count(_.pass == Left(Dragon)))
      // Pass card now in hand.
      assertResult(14)(moves.iterator.count(_.p1.contains(Horse)))
    }
  }
}
