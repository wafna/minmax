package wafna
package games
package onitama

import wafna.games.minmax.MinMax
import wafna.games.onitama.Deck.*
import wafna.games.onitama.OnitamaMinMax.*

import scala.collection.immutable.ArraySeq

class Scenario1Test extends TestBase {
  "Scenario 1" should {
    "P1 to win in one move" in {

      // P1 can kill king with Eel.
      val g0 = new Onitama(
        Hand(Tiger, Eel),
        Hand(Rabbit, Crab),
        TurnP1(Rooster),
        new Board(
          ArraySeq
            .fill(25)(Option.empty[Piece])
            .updated(Spot(3, 2).toIx, Some(Piece(P1, King)))
            .updated(Spot(2, 3).toIx, Some(Piece(P2, King)))
        )
      )

      println(Console.show(g0).mkString("\n"))
      g0.moves().toList.foreach { move =>
        println(Console.show(move).mkString("\n"))
      }

      assertResult(P1)(g0.currentPlayer)

      assertResult(1)(g0.moves().toList.count { move =>
        move.turnInHand.card == Eel && move.gameOver.contains(Win(P1))
      })

      def evaluate(game: Onitama, player: Player): Int = game.gameOver match {

        case Some(Draw) =>
          // Even if the player cannot move a piece the player must still exchange a card.
          sys.error("Draw disallowed.")

        case Some(Win(p)) =>
          if (p == player) Int.MaxValue else Int.MinValue

        case None => 0
      }
      // [2022-2-6] Works at depth 4, fails at depth 5!
      // also fails if there is at least one of either player's pawns.
      // also it seems to matter where the pawns are.
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
          gf.turnInHand.card shouldBe Eel
      }
    }
  }
}
