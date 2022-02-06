package wafna
package games
package onitama

import wafna.games.minmax.MinMax
import wafna.games.onitama.Deck.*
import wafna.games.onitama.OnitamaMinMax.*

import scala.collection.immutable.ArraySeq

class Scenario1Test extends TestBase {
  "Scenario 1" should {
    /*
      This is the scene:

      P1: Tiger, Eel
      P2: Rabbit, Crab
      Pass: P1 Rooster

      oo-oo
      .....
      ..O..
      ...Xx
      xx-x.

    Here are the cards:

      Tiger    Crab     Rabbit   Rooster  Eel
        X
                 X         X        X      X
        O      X O X      O X     XOX       OX
        X                X        X        X

    X (P1) can use Eel to kill O's (P2) king and win.

     */
    "P1 to win in one move" in {
      val g0 = new Onitama(
        Hand(Tiger, Eel),
        Hand(Rabbit, Crab),
        Right(Rooster),
        new Board(
          ArraySeq(
            Some(Piece(P2, Pawn)),
            Some(Piece(P2, Pawn)),
            None,
            Some(Piece(P2, Pawn)),
            Some(Piece(P2, Pawn)),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(Piece(P2, King)),
            None,
            None,
            None,
            None,
            None,
            Some(Piece(P1, King)),
            Some(Piece(P1, Pawn)),
            Some(Piece(P1, Pawn)),
            Some(Piece(P1, Pawn)),
            None,
            Some(Piece(P1, Pawn)),
            None
          )
        )
      )

      assertResult(P1)(g0.currentPlayer)

      def evaluate(game: Onitama, player: Player): Int = game.gameOver match {
        case Some(Draw) =>
          // Even if the player cannot move a piece the player must still exchange a card.
          sys.error("Draw disallowed.")

        case Some(Win(p)) =>
          if (p == player) Int.MaxValue else Int.MinValue

        case None =>
          0
      }
      // [2022-2-6] Works at depth 4, fails at depth 5!
      val result: Either[GameOver, MinMax.Eval[Onitama]] = {
        MinMax.search(g0, 5, evaluate)
      }
      result match {

        case Left(_) =>
          fail("Game is over.")

        case Right(eval) =>

          val gf = eval.game

          // P1 has won.
          gf.gameOver shouldBe Some(Win(P1))
          // By using Eel to take P2's King.
          gf.pass shouldBe Left(Eel)
      }
    }
  }
}
