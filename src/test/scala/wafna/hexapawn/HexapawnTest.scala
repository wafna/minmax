package wafna.hexapawn

import wafna.TestBase
import wafna.hexapawn.Hexapawn.DrawCond
import wafna.util.Player
import wafna.util.Player.{P1, P2}

class HexapawnTest extends TestBase {
  "A new 3x3" should {
    val (cols, rows) = (3, 3)
    val g0 = Hexapawn(rows, cols)
    "have valid starting positions" in {
      (0 to 2).foreach { col =>
        g0.spot(col, 0) shouldBe Some(P1)
        g0.spot(col, 1) shouldBe None
        g0.spot(col, 2) shouldBe Some(P2)
      }
    }
    "have three opening moves" in {
      g0.nextMoves.length shouldBe 3
    }
    "have no winner" in {
      g0.winner shouldBe None
    }
  }
  "hexapawn" should {
    "should correctly detect win cons for P1" in {
      Seq(2, 5, 8).foreach { p =>
        val spots = Array.fill(9)(None: Option[Player])
        spots(p) = Some(P1)
        val g = new Hexapawn(P2, 3, 3, DrawCond.Open, spots)
        g.winner shouldBe Some(Some(P1))
      }
    }
    "should correctly detect win cons for P2" in {
      Seq(0, 3, 6).foreach { p =>
        val spots = Array.fill(9)(None: Option[Player])
        spots(p) = Some(P2)
        val g = new Hexapawn(P1, 3, 3, DrawCond.Open, spots)
        g.winner shouldBe Some(Some(P2))
      }
    }
  }
}
