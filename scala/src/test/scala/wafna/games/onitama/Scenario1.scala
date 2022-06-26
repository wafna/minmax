package wafna
package games
package onitama

import wafna.games.minmax.MinMax
import wafna.games.onitama.Deck.*
import wafna.games.onitama.OnitamaMinMax.*

import scala.collection.immutable.ArraySeq

class Scenario1 extends ScenarioTestBase {

  test("Player 1 to win in one move") {

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

      assertResult(P1)(g0.currentPlayer)

      assertResult(1)(g0.moves().toList.count { move =>
        move.turnInHand.card == Eel && move.gameOver.contains(Win(P1))
      })

      MinMax.search(g0, 5, evaluate) match {

        case Left(_) =>
          fail("Game is over.")

        case Right(eval) =>
          val gf = eval.game

          // P1 has won.
          assertResult(Some(Win(P1)))(gf.gameOver)
          // By using Eel to take P2's King.
          assertResult(Eel)(gf.turnInHand.card)
      }
    }
}
