package wafna.minmax

import org.scalatest.Assertion
import wafna.TestBase
import wafna.minmax.MinMax.State
import wafna.util.Player
import wafna.util.Player.{P1, P2}

class MinMaxSpec extends TestBase {

  import wafna.minmax.MinMaxSpec._

  "MinMax" should {
    def testSearch(depth: Int)(expectedGameId: String)(game: Game): Assertion = {
      MinMax.search(game, depth).get.game.id shouldBe expectedGameId
    }
    "select the best move" in {
      testSearch(1)("1-2") {
        // format: off
        Game("0", None, P1, Right(List(
          Game("1-1", Some(0), P2),
          Game("1-2", Some(1), P2),
          Game("1-3", Some(-1), P2)
        )))
        // format: on
      }
      testSearch(1)("1-4") {
        // format: off
        Game("0", None, P1, Right(List(
          Game("1-1", Some(0), P2),
          Game("1-2", Some(1), P2),
          Game("1-3", Some(-1), P2),
          Game("1-4", Some(2), P2)
        )))
        // format: on
      }
    }
    "prune the tree" in {
      testSearch(2)("1-1") {
        // format: off
        Game("1", None, P1, Right(List(
          Game("1-1", None, P2, Right(List(
            Game("1-1-1", Some(-1), P1)
          ))),
          Game("1-2", None, P2, Right(List(
            Game("1-2-1", Some(0), P1),
            Game("triggers a prune", Some(-2), P1),
            Game("pruned: do not evaluate!", None, P1)
          )))
        )))
        // format: on
      }
    }
    "evaluate early" in {
      testSearch(2)("1-1") {
        // format: off
        Game("1", None, P1, Right(List(
          Game("1-1", Some(-1), P2),
          Game("1-2", None, P2, Right(List(
            Game("1-2-1", Some(0), P1),
            Game("triggers a prune", Some(-2), P1),
            Game("pruned: do not evaluate!", None, P1)
          )))
        )))
        // format: on
      }
    }
  }
}

object MinMaxSpec {

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
