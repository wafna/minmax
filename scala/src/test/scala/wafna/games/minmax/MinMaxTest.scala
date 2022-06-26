package wafna
package games
package minmax

import cats.data.NonEmptyList
import org.scalatest.Assertion

class MinMaxTest extends TestBase {

  import wafna.games.minmax.MinMaxTest.*

  "MinMax" should {

    def testSearch(depth: Int)(expectedResult: Either[GameOver, (String, Int)])(game: Game): Assertion = {

      def evaluate(game: Game, player: Player): Int =
        game.next match {
          case Evaluate(value) => value
          case Moves(Left(gameOver)) =>
            gameOver match {
              case Draw   => 0
              case Win(p) => if (p == player) 100 else -100
            }
          case Error(message) => sys.error(s"Game '${game.id}' evaluated in error: $message")
          case _              => sys.error(s"Illegal evaluation at game '${game.id}'")
        }

      MinMax.search(game, depth, evaluate).map(e => e.game.id -> e.eval) shouldBe expectedResult
    }

    // This is to ensure there's no hard coded bias for a player, e.g. 'P1' instead of 'p1'.
    val bothWays: List[(Player, Player)] = List(P1 -> P2, P2 -> P1)

    "evaluates to max depth only" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1" -> 0)) {
          // format: off
          Game("0", p1, Moves(
            Game("1", p2, Moves(
              Game("1-1", p1, Evaluate(0))
            ))
          ))
          // format: on
        }
      }
    }
    "terminates early" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1" -> 0)) {
          // format: off
          Game("0", p1, Moves(
            Game("1", p2, Moves(Draw))
          ))
          // format: on
        }
      }
    }
    "select the best move" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Right("2" -> 1)) {
          // format: off
        Game("0", p1, Moves(
          Game("1", p2, Moves(Draw)),
          Game("2", p2, Evaluate(1)),
          Game("3", p2, Evaluate(-1))
        ))
        // format: on
        }
      }
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Right("4" -> 2)) {
          // format: off
        Game("0", p1, Moves(
          Game("1", p2, Moves(Draw)),
          Game("2", p2, Evaluate(1)),
          Game("3", p2, Evaluate(-1)),
          Game("4", p2, Evaluate(2))
        ))
        // format: on
        }
      }
    }
    "prune the tree" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1-1" -> -1)) {
          // format: off
          Game("0", p1, Moves(
            Game("1-1", p2, Moves(
              Game("1-1-1", p1, Evaluate(-1))
            )),
            Game("2", p2, Moves(
              Game("2-1", p1, Moves(Draw)),
              Game("2-2 (triggers a prune)", p1, Moves(Win(p2))),
              Game("2-3", p1, Error("pruned: do not evaluate!"))
            ))
          ))
          // format: on
        }
      }
    }
    "depth 2 best move" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("2" -> 2)) {
          // format: off
          Game("0", p1, Moves(
            Game("1", p2, Moves(
              Game("1-1", p1, Evaluate(1)),
              Game("1-2", p1, Moves(Draw))
            )),
            Game("2", p2, Moves(
              Game("2-2", p1, Evaluate(2)),
              Game("2-3", p1, Evaluate(3))
            )),
            Game("3", p2, Moves(
              Game("3-1", p1, Evaluate(-2)),
              Game("3-2", p1, Moves(Draw))
            ))
          ))
          // format: on
        }
      }
    }
    "depth 2 early winner" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1" -> 100)) {
          // format: off
          Game("0", p1, Moves(
            Game("1", p2, Moves(Win(p1))),
            Game("2", p2, Moves(
              Game("2-2", p1, Evaluate(2)),
              Game("2-3", p1, Evaluate(3))
            )),
            Game("3", p2, Moves(
              Game("3-1", p1, Evaluate(-2)),
              Game("3-2", p1, Moves(Draw))
            ))
          ))
          // format: on
        }
      }
    }
    // The point here is to ensure that a path to a win deep in the tree doesn't mask
    // an move to force a win.
    "depth 2 other winner" in {
      bothWays.foreach { case (p1, p2) =>
        // format: off
        val g1 = Game("1", p2, Moves(
          Game("1-1", p1, Moves(
            Game("1-1-1", p2, Moves(Win(p1)))
          ))
        ))
        val g2 = Game("2", p2, Moves(
          Game("2-1", p1, Moves(Draw)),
          Game("2-2", p1, Moves(
            Game("2-2-1", p2, Moves(Win(p1)))
          ))
        ))
        // format: on
        testSearch(3)(Right("1" -> 100)) {
          Game("0", p1, Moves(g1, g2))
        }
        testSearch(3)(Right("1" -> 100)) {
          Game("0", p1, Moves(g2, g1))
        }
      }
    }
    "avoid losing" in {
      bothWays.foreach { case (p1, p2) =>
        // format: off
        val g1 = Game("1", p2, Moves(
          Game("1-1", p1, Moves(Win(p2)))
        ))
        val g2 = Game("2", p2, Moves(
          Game("2-1", p1, Moves(Draw))
        ))
        // format: on
        testSearch(3)(Right("2" -> 0)) {
          Game("0", p1, Moves(g1, g2))
        }
        testSearch(3)(Right("2" -> 0)) {
          Game("0", p1, Moves(g2, g1))
        }
      }
    }
  }
}

object MinMaxTest {

  sealed trait Next
  case class Evaluate(value: Int) extends Next
  case class Moves(moves: Either[GameOver, NonEmptyList[Game]]) extends Next
  object Moves {
    def apply(moves: Game*): Moves = Moves(Right(NonEmptyList.fromListUnsafe(moves.toList)))
    def apply(gameOver: GameOver): Moves = Moves(Left(gameOver))
  }
  case class Error(msg: String = "just don't") extends Next

  case class Game(id: String, player: Player, next: Next)

  implicit val gameTreeMinMax: MinMax[Game] = new MinMax[Game] {
    override def currentPlayer(game: Game): Player = game.player
    override def moves(game: Game): Either[GameOver, NonEmptyList[Game]] =
      game.next match {
        case Moves(moves) => moves
        case wat: Any     => sys.error(s"No moves: $wat")
      }
  }
}
