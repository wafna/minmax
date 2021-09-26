package wafna.minmax

import org.scalatest.Assertion
import wafna.TestBase
import wafna.minmax.MinMax.State
import wafna.minmax.MinMax.State.{Drawn, Won}
import wafna.util.Player
import wafna.util.Player.{P1, P2}

class MinMaxTest extends TestBase {

  import wafna.minmax.MinMaxTest._

  "MinMax" should {
    def testSearch(depth: Int)(expectedGameId: Option[String])(game: Game): Assertion = {
      MinMax.search(game, depth).map(_.game.id) shouldBe expectedGameId
    }
    val bothWays: List[(Player, Player)] = List(P1 -> P2, P2 -> P1)

    "select the best move" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Some("2")) {
          // format: off
        Game("0", None, p1, Right(List(
          Game("1", Some(0), p2),
          Game("2", Some(1), p2),
          Game("3", Some(-1), p2)
        )))
        // format: on
        }
      }
      bothWays.foreach { case (p1, p2) =>
        testSearch(1)(Some("4")) {
          // format: off
        Game("0", None, p1, Right(List(
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
        testSearch(2)(Some("1")) {
          // format: off
          Game("1", None, p1, Right(List(
            Game("1", None, p2, Right(List(
              Game("1", Some(-1), p1)
            ))),
            Game("2", None, p2, Right(List(
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
        testSearch(2)(Some("1")) {
          // format: off
          Game("1", None, p1, Right(List(
            Game("1", Some(-1), p2),
            Game("2", None, p2, Right(List(
              Game("2-1", Some(0), p1),
              Game("triggers a prune", Some(-2), p1),
              Game("pruned: do not evaluate!", None, p1)
            )))
          )))
          // format: on
        }
      }
    }
    "fail on invalid game state" in {
      bothWays.foreach { case (p1, _) =>
        testSearch(1)(None) {
          Game("1", None, p1, Right(Nil))
        }
      }
    }
    "evaluates to max depth only" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Some("1")) {
          // format: off
          Game("1", None, p1, Right(List(
            Game("1", None, p2, Right(List(
              Game("1-1", Some(0), p1, Right(List(
                Game("do_not_evaluate", None, p2, Right(Nil))
              )))
            )))
          )))
          // format: on
        }
      }
    }
    "depth 2 something" in {
      bothWays.foreach { case (p1, p2) =>
        testSearch(2)(Some("2")) {
          // format: off
          Game("1", None, p1, Right(List(
            Game("1", None, p2, Right(List(
              Game("1-1", Some(-1), p1, Left(Won(p2))),
              Game("1-2", Some(0), p1, Right(Nil))
            ))),
            Game("2", None, p2, Right(List(
              Game("2-1", Some(2), p1, Right(Nil)),
              Game("2-1", Some(3), p1, Right(Nil))
            ))),
            Game("3", None, p2, Right(List(
              Game("3-1", Some(-2), p1, Right(Nil)),
              Game("3-1", None, p1, Right(Nil))
            )))
          )))
          // format: on
        }
      }
    }
  }
}

object MinMaxTest {

  case class Game(id: String, eval: Option[Int], player: Player, state: Either[State, List[Game]] = Left(State.Drawn))

  implicit val gameTreeMinMax: MinMax[Game] = new MinMax[Game] {
    override def currentPlayer(game: Game): Player = game.player
    override def evaluate(game: Game, player: Player): Int =
      game.eval.getOrElse(sys.error(s"Illegal evaluation at game '${game.id}''"))
    override def moves(game: Game): Seq[Game] =
      game.state.getOrElse(sys.error(s"No moves: '${game.id}' ${game.state}"))
    override def pass(game: Game): Game =
      game.copy(player = game.player.opponent)
    override def state(game: Game): State =
      game.state.swap.getOrElse(State.Open)
  }
}
