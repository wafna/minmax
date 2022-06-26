package wafna
package games
package onitama

class BoardTest extends TestBase {

  implicit class MoveMustSucceed(result: Option[Board]) {
    def insist(): Board = result.getOrElse(fail("Move must succeed."))
  }

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
    "Find pieces" in {
      val board0 = Board()
      board0.occupied(P1).size shouldBe 5
      board0.occupied(P2).size shouldBe 5
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
  "Game Over" in {
    val board0 = Board()

    val kingDeadP2 = board0.move(Spot(0, 0), Move(2, 4)).insist()
    kingDeadP2.gameOver shouldBe Some(Win(P1))
    val kingDeadP1 = board0.move(Spot(0, 4), Move(2, -4)).insist()
    kingDeadP1.gameOver shouldBe Some(Win(P2))

    val throneUsurpedP2 = board0.move(Spot(2, 4), Move(-1, -1)).insist().move(Spot(2, 0), Move(0, 4)).insist()
    throneUsurpedP2.gameOver shouldBe Some(Win(P1))
    val throneUsurpedP1 = board0.move(Spot(2, 0), Move(-1, 1)).insist().move(Spot(2, 4), Move(0, -4)).insist()
    throneUsurpedP1.gameOver shouldBe Some(Win(P2))
  }
}
