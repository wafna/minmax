package wafna.minmax

import org.scalatest.Assertion
import wafna.util.Player
import wafna.util.Player.{P1, P2}
import wafna.{TestBase, util}

class MinMaxSpec extends TestBase {

  import wafna.minmax.MinMaxSpec._

  implicit val mm: MinMax[Game] = GameTreeMinMax
  "MinMax" should {
    "select the best move" in {
      def runTree(depth: Int)(expected: String)(tree: Game): Assertion = {
        expected shouldBe MinMax.search(tree, depth).get.game.id
      }
//      runTree(1)("1-2") {
//        // format: off
//        GameTree("0", 0, P1, List(
//          GameTree("1-1", 0, P2, List()),
//          GameTree("1-2", 1, P2, List()),
//          GameTree("1-3", -1, P2, List())
//        ))
//        // format: on
//      }
//      runTree(1)("1-4") {
//        // format: off
//        GameTree("0", 0, P1, List(
//          GameTree("1-1", 0, P2, List.empty),
//          GameTree("1-2", 1, P2, List.empty),
//          GameTree("1-3", -1, P2, List.empty),
//          GameTree("1-4", 2, P2, List.empty)
//        ))
//        // format: on
//      }
      runTree(2)("1-1") {
        // format: off
        Game("1", 0, P1, List(
          Game("1-1", 0, P2, List(
            Game("1-1-1", -1, P1, List.empty)
          )),
          Game("1-2", 0, P2, List(
            Game("1-2-1", -2, P1, List.empty),
            Game("1-2-2", 1, P1, List.empty)
          ))
        ))
        // format: on
      }
    }
  }
}

object MinMaxSpec {

  case class Game(id: String, eval: Int, player: Player, moves: List[Game])

  implicit object GameTreeMinMax extends MinMax[Game] {
    override def currentPlayer(game: Game): util.Player = game.player
    override def evaluate(game: Game, player: util.Player): Int = game.eval
    override def moves(game: Game): Seq[Game] = game.moves.toSeq
    override def pass(game: Game): Game = ???
    override def show(game: Game): Option[String] =
      Some(s"game: id=${game.id}, p=${game.player}. eval=${game.eval}")
  }
}
