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
    val g0 = new Onitama(
      Hand(Tiger, Rabbit),
      Hand(Goose, Elephant),
      TurnP2(Frog),
      Board()
        .move(Spot(4, 0), Move(-3, 1))
        .get
        .move(Spot(2, 4), Move(0, -2))
        .get
    )

    println(Console.show(g0.cards).mkString("\n"))
    println(Console.show(g0).mkString("\n"))

    assertResult(P2)(g0.currentPlayer)

    MinMax.search(g0, 5, evaluate) match {

      case Left(_) =>
        fail("Game is over.")

      case Right(eval) =>
        val gf = eval.game
        println(Console.show(gf).mkString("\n"))
    }
  }
}
