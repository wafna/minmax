package wafna
package games
package minmax

import cats.data.NonEmptyList
import org.scalatest.Assertion

class MinMaxTest extends TestBase {

  import wafna.games.minmax.MinMaxTest._

  "MinMax" should {
    def testSearch(depth: Int)(expectedGameId: Either[GameOver, String])(game: Game): Assertion = {
      def evaluate(game: Game, player: Player): Int =
        game.eval.getOrElse(sys.error(s"Illegal evaluation at game '${game.id}''"))
      MinMax.search(game, depth, evaluate).map(_.game.id) shouldBe expectedGameId
    }
    val bothWays: List[(Player, Player)] = List(P1 -> P2, P2 -> P1)

    "take a draw" in {
      bothWays.foreach { case (p1, _) =>
        testSearch(1)(Left(Draw)) {
          Game("1", None, p1, Left(Draw))
        }
      }
    }
    "select the best move" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Right("2")) {
          // format: off
        Game("0", None, p1, Right(nonEmptyList(
          Game("1", Some(0), p2),
          Game("2", Some(1), p2),
          Game("3", Some(-1), p2)
        )))
        // format: on
        }
      }
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Right("4")) {
          // format: off
        Game("0", None, p1, Right(nonEmptyList(
          Game("1", Some(0), p2),
          Game("2", Some(1), p2),
          Game("3", Some(-1), p2),
          Game("4", Some(2), p2)
        )))
        // format: on
        }
      }
    }
    "prune the tree" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1")) {
          // format: off
          Game("1", None, p1, Right(nonEmptyList(
            Game("1", None, p2, Right(nonEmptyList(
              Game("1", Some(-1), p1)
            ))),
            Game("2", None, p2, Right(nonEmptyList(
              Game("2-1", Some(0), p1),
              Game("triggers a prune", Some(-2), p1),
              Game("pruned: do not evaluate!", None, p1)
            )))
          )))
          // format: on
        }
      }
    }
    "evaluate early" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1")) {
          // format: off
          Game("1", None, p1, Right(nonEmptyList(
            Game("1", Some(-1), p2),
            Game("2", None, p2, Right(nonEmptyList(
              Game("2-1", Some(0), p1),
              Game("triggers a prune", Some(-2), p1),
              Game("pruned: do not evaluate!", None, p1)
            )))
          )))
          // format: on
        }
      }
    }
    "evaluates to max depth only" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("1")) {
          // format: off
          Game("1", None, p1, Right(nonEmptyList(
            Game("1", None, p2, Right(nonEmptyList(
              Game("1-1", Some(0), p1, Right(nonEmptyList(
                Game("do_not_evaluate", None, p2, Left(Draw))
              )))
            )))
          )))
          // format: on
        }
      }
    }
    "depth 2 something" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Right("2")) {
          // format: off
          Game("1", None, p1, Right(nonEmptyList(
            Game("1", None, p2, Right(nonEmptyList(
              Game("1-1", Some(-1), p1, Left(Win(p2))),
              Game("1-2", Some(0), p1, Left(Draw))
            ))),
            Game("2", None, p2, Right(nonEmptyList(
              Game("2-1", Some(2), p1, Left(Draw)),
              Game("2-1", Some(3), p1, Left(Draw))
            ))),
            Game("3", None, p2, Right(nonEmptyList(
              Game("3-1", Some(-2), p1, Left(Draw)),
              Game("3-1", None, p1, Left(Draw))
            )))
          )))
          // format: on
        }
      }
    }
  }
}

object MinMaxTest {

  case class Game(
    id: String,
    eval: Option[Int],
    player: Player,
    state: Either[GameOver, NonEmptyList[Game]] = Left(Draw)
  )

  implicit val gameTreeMinMax: MinMax[Game] = new MinMax[Game] {
    override def currentPlayer(game: Game): Player = game.player
    override def moves(game: Game): Either[GameOver, NonEmptyList[Game]] =
      game.state
  }
}
