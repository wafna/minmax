package wafna.games
package minmax

import wafna.games.Player.{P1, P2}
import wafna.games.minmax.MinMax.{Eval, Evaluator}

import scala.annotation.tailrec
import scala.util.Random
import scala.util.control.NoStackTrace

object Arena {

  class ArenaException(msg: String) extends Exception(msg) with NoStackTrace
  object ArenaException {
    def apply(msg: String): Nothing = throw new ArenaException(msg)
  }

  abstract class Bot[G](implicit minMax: MinMax[G]) {
    def show(): String
    def move(game: G): Either[GameOver, Eval[G]]
  }

  abstract class SearchBot[G, L <: MinMax.Listener](depth: Int)(implicit
    minMax: MinMax[G],
    listener: L = MinMax.ListenerNoOp
  ) extends Bot[G] {

    override def show(): String = s"Search($depth)"
    override def move(game: G): Either[GameOver, Eval[G]] = MinMax.search(game, depth, evaluate)
    def evaluate(game: G, player: Player): Int

    def getListener: L = listener
  }

  class RandomBot[G]()(implicit minMax: MinMax[G], random: Random = Random) extends Bot[G] {
    override def show(): String = "Random"
    override def move(game: G): Either[GameOver, Eval[G]] = {
      minMax.moves(game).flatMap { moves =>
        Right(Eval(moves.toList(new Random().nextInt(moves.length)), 0))
      }
    }
  }

  def runGame[G](game: G, p1: Bot[G], p2: Bot[G])(implicit minMax: MinMax[G]): (GameOver, List[G]) = {
    @tailrec
    def runPlayer(game: G, bots: LazyList[Bot[G]], moves: List[G])(implicit minMax: MinMax[G]): (GameOver, List[G]) = {
      bots.head.move(game) match {
        case Left(gameOver) =>
          (gameOver, game :: moves)
        case Right(move) =>
          runPlayer(move.game, bots.tail, game :: moves)
      }
    }
    runPlayer(game, LazyList.continually(Seq(p1, p2)).flatten, Nil)
  }

  case class Match(p1: String, p2: String, wins1: Int, wins2: Int, draws: Int)

  def runMatch[G](game: G, p1: Bot[G], p2: Bot[G], games: Int)(implicit minMax: MinMax[G]): Match = {
    require(0 < games)
    def keepScore(result: (GameOver, List[G])): (Int, Int, Int) = {
      result match {
        case (Draw, _) => (0, 0, 1)
        case (Win(winner), _) =>
          winner match {
            case P1 => (1, 0, 0)
            case P2 => (0, 1, 0)
          }

      }
    }
    val score = Iterator.continually(game).take(games).foldLeft((0, 0, 0)) { (score, game) =>
      val score1 = keepScore(runGame(game, p1, p2))
      val score2 = keepScore(runGame(game, p2, p1))
      (score._1 + score1._1 + score2._2, score._2 + score1._2 + score2._1, score._3 + score1._3 + score2._3)
    }
    Match(p1.show(), p2.show(), score._1, score._2, score._3)
  }
}
