package wafna.minmax

import org.scalatest.Assertion
import wafna.util.Player
import wafna.util.Player.{P1, P2}
import wafna.{TestBase, util}

class MinMaxSpec extends TestBase {

  import wafna.minmax.MinMaxSpec._

  "MinMax" should {
    def testSearch(depth: Int)(expectedGameId: String)(game: Game): Assertion = {
      MinMax.search(game, depth).get.game.id shouldBe expectedGameId
    }
    "select the best move" in {
      testSearch(1)("1-2") {
        // format: off
        Game("0", None, P1, List(
          Game("1-1", Some(0), P2),
          Game("1-2", Some(1), P2),
          Game("1-3", Some(-1), P2)
        ))
        // format: on
      }
      testSearch(1)("1-4") {
        // format: off
        Game("0", None, P1, List(
          Game("1-1", Some(0), P2),
          Game("1-2", Some(1), P2),
          Game("1-3", Some(-1), P2),
          Game("1-4", Some(2), P2)
        ))
        // format: on
      }
    }
    "prune the tree" in {
      testSearch(2)("1-1") {
        // format: off
        Game("1", None, P1, List(
          Game("1-1", None, P2, List(
            Game("1-1-1", Some(-1), P1)
          )),
          Game("1-2", None, P2, List(
            Game("1-2-1", Some(0), P1),
            Game("triggers a prune", Some(-2), P1),
            Game("pruned: do not evaluate!", None, P1)
          ))
        ))
        // format: on
      }
    }
    "evaluate early" in {
      testSearch(2)("1-2") {
        // format: off
        Game("1", None, P1, List(
          Game("no children: evaluate immediately", Some(-3), P2),
          Game("1-2", None, P2, List(
            Game("1-2-1", Some(0), P1),
            Game("triggers a prune", Some(-2), P1),
            Game("pruned: do not evaluate!", None, P1)
          ))
        ))
        // format: on
      }
    }
  }
}

object MinMaxSpec {

  sealed trait Node
  case class Game(id: String, eval: Option[Int], player: Player, moves: List[Game] = List.empty) extends Node

  implicit val gameTreeMinMax: MinMax[Game] = new MinMax[Game] {
    override def currentPlayer(game: Game): util.Player = game.player
    override def evaluate(game: Game, player: util.Player): Int =
      game.eval.getOrElse(sys.error(s"Illegal evaluation at game ${game.id}"))
    override def moves(game: Game): Seq[Game] = game.moves.toSeq
    override def pass(game: Game): Game = ???
    override def winner(game: Game): Option[Player] = ???
  }
}
