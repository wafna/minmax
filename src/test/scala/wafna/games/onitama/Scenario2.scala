package wafna
package games
package onitama

import wafna.games.minmax.MinMax
import wafna.games.onitama.Deck.*
import wafna.games.onitama.OnitamaMinMax.*

import scala.collection.immutable.ArraySeq

class Scenario2 extends ScenarioTestBase {

  test("Player 2 should avoid losing its King") {

    // P1 can kill P2's king with Rabbit, so P2 must avoid this fate.
    // P2 can use Goose or Elephant to slide left or it can use Goose to retreat.
    val g0 = new Onitama(
      Hand(Tiger, Rabbit),
      Hand(Goose, Elephant),
      TurnP2(Frog),
      new Board(
        Board()
          .move(Spot(4, 0), Move(-3, 1))
          .get
          .move(Spot(2, 4), Move(0, -2))
          .get
          .spots
          .updated(Spot(0, 0).toIx, None)
          .updated(Spot(0, 4).toIx, None)
          .updated(Spot(3, 0).toIx, None)
          .updated(Spot(1, 4).toIx, None)
          .updated(Spot(3, 4).toIx, None)
          .updated(Spot(2, 0).toIx, None)
          .updated(Spot(1, 0).toIx, Some(Piece(P1, King)))
      )
    )

    println(Console.show(g0.cards).mkString("\n"))
    println(Console.show(g0).mkString("\n"))

    assertResult(P2)(g0.currentPlayer)

    implicit val listener: MinMax.ListenerCounter = MinMax.ListenerCounter("scenario-2")
    
    MinMax.search(g0, 5, evaluate) match {

      case Left(_) =>
        fail("Game is over.")

      case Right(eval) =>
        val gf = eval.game
        println()
        println(Console.show(gf).mkString("\n"))
        println()
        println(Console.showStats(listener.stats()))

        assertResult(P1)(gf.currentPlayer)

        // todo for now, we're asserting that P2's king hasn't moved while we pare down the board.
        assertResult(Some(Piece(P2, King)))(gf.board.spot(Spot(2, 2)))
    }
  }
}
