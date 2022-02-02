package wafna.games.onitama

import wafna.TestBase
import wafna.util.Player.{P1, P2}

class BoardTest extends TestBase {

  "Board" should {
    "Start" in {
      val board = Board()
      Seq(0, 1, 3, 4).foreach { x =>
        board.spot(Spot(x, 0)) shouldBe Some(Piece(P1, Pawn))
        board.spot(Spot(x, 4)) shouldBe Some(Piece(P2, Pawn))
      }
      board.spot(Spot(2, 0)) shouldBe Some(Piece(P1, King))
      board.spot(Spot(2, 4)) shouldBe Some(Piece(P2, King))
      (1 to 3).foreach { y =>
        (0 to 4).foreach { x =>
          board.spot(Spot(x, y)) shouldBe None
        }
      }
    }
    "Move" in {
      val board0 = Board()
      // move to open square
      val board1 = board0.move(Spot(2, 0), Move(0, 2)).getOrElse(fail("Move should succeed."))
      board1.spot(Spot(2, 0)) shouldBe None
      board1.spot(Spot(2, 2)) shouldBe Some(Piece(P1, King))
      // forbid moves out of bounds or onto own piece.
      board1.move(Spot(3, 4), Move(0, 1)) shouldBe None
      board1.move(Spot(3, 4), Move(1, 0)) shouldBe None
      board1.move(Spot(3, 4), Move(-1, 0)) shouldBe None
      // capture
      val board2 = board1.move(Spot(0, 0), Move(0, 4)).getOrElse(fail("Move should succeed."))
      board2.spot(Spot(0, 0)) shouldBe None
      board2.spot(Spot(0, 4)) shouldBe Some(Piece(P1, Pawn))
    }
  }
}
