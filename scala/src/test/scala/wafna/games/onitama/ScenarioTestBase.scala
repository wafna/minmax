package wafna
package games
package onitama

import org.scalatest.funsuite.AnyFunSuite

abstract class ScenarioTestBase extends AnyFunSuite {

  def evaluate(game: Onitama, player: Player): Int = game.gameOver match {

    case Some(Draw) =>
      // Even if the player cannot move a piece the player must still exchange a card.
      sys.error("Draw disallowed.")

    case Some(Win(p)) =>
      if (p == player) Int.MaxValue else Int.MinValue

    case None => 0
  }
}
