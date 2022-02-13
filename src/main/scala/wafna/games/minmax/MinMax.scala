package wafna.games
package minmax

import cats.data.NonEmptyList
import nl.grons.metrics4.scala.{Counter, DefaultInstrumented, Meter, Timer}
import wafna.games.Player

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

/** Type class of MinMax games. */
trait MinMax[G] {

  /** The player with turn in hand for the game.
    */
  def currentPlayer(game: G): Player

  /** Return a non-empty list of moves from a game state or declare the game to be over.
    * Passes are modeled by having returned moves have the same moving player as the originating game.
    */
  def moves(game: G): Either[GameOver, NonEmptyList[G]]
}

object MinMax {

  class MinMaxError(msg: String) extends Exception(msg)

  object MinMaxError {
    def apply(msg: String): Nothing = throw new MinMaxError(msg)
  }

  trait Listener[G] {

    def search(searchingPlayer: Player, game: G, prune: Option[Int], depth: Int): Unit
    def prune(mm: Int, prune: Int, eval: Int): Unit
    def evaluate(eval: => Int): Int = eval
  }

  class ListenerNoOp[G] extends Listener[G] {

    override def search(searchingPlayer: Player, game: G, prune: Option[Int], depth: Int): Unit = ()
    override def prune(mm: Int, prune: Int, eval: Int): Unit = ()
    override def evaluate(eval: => Int): Int = eval
  }

  case class MeterSnapshot(count: Long, meanRate: Double)
  object MeterSnapshot {
    def apply(meter: Meter): MeterSnapshot =
      MeterSnapshot(meter.count, meter.meanRate)
  }

  case class TimerSnapshot(count: Long, average: Double, rate: Double)
  object TimerSnapshot {
    def apply(timer: Timer): TimerSnapshot =
      TimerSnapshot(timer.count, timer.mean, timer.meanRate)
  }

  case class Stats(searches: MeterSnapshot, evaluations: TimerSnapshot, prunes: Long, evalWins: Long, evalLosses: Long)

  class ListenerCounter[G](name: String) extends Listener[G] with DefaultInstrumented {
    private val searches = metrics.meter(s"searches-$name")
    private val evaluations = metrics.timer(s"evaluations-$name")
    private val prunes = metrics.counter(s"prunes-$name")
    private val evalWins = metrics.counter(s"eval-wins-$name")
    private val evalLosses = metrics.counter(s"eval-losses-$name")

    override def search(searchingPlayer: Player, game: G, prune: Option[Int], depth: Int): Unit = searches.mark()
    override def evaluate(eval: => Int): Int = {
      val now = System.currentTimeMillis()
      val v = eval
      if (v == Int.MinValue) {
        evalLosses.inc()
      } else if (v == Int.MaxValue) {
        evalWins.inc()
      }
      evaluations.update(now - System.currentTimeMillis(), TimeUnit.MILLISECONDS)
      v
    }
    override def prune(mm: Int, prune: Int, eval: Int): Unit = prunes.inc()

    def stats(): Stats =
      Stats(MeterSnapshot(searches), TimerSnapshot(evaluations), prunes.count, evalWins.count, evalLosses.count)
  }

  /** A game plus its valuation relative to the searching player. */
  final case class Eval[G](game: G, eval: Int)

  type Evaluator[G] = (G, Player) => Int

  /** Search the current game to the specified depth for the best move.
    */
  //noinspection ScalaStyle
  def search[G](game: G, maxDepth: Int, evaluator: Evaluator[G])(implicit
    minMax: MinMax[G],
    listener: Listener[G] = new ListenerNoOp[G]
  ): Either[GameOver, Eval[G]] = {
    require(0 < maxDepth, "maxDepth must be positive.")
    // This player is the player initiating the search, throughout.
    val searchingPlayer = minMax.currentPlayer(game)

    @inline def selectBest[G](mm: Int, best: Option[Eval[G]], maybe: Eval[G]): Option[Eval[G]] =
      if (best.exists(mm * _.eval > mm * maybe.eval)) {
        best
      } else {
        Some(maybe)
      }

    /** Below the first layer of search we may have a pruning value which we use to abort unfruitful searches.
      */
    def searchPruned(game: G, prune: Option[Int], depth: Int)(implicit minMax: MinMax[G], listener: Listener[G]): Int = {

      if (0 == depth) {
        listener.evaluate(evaluator(game, searchingPlayer))
      } else {
        listener.search(searchingPlayer, game, prune, maxDepth - depth)
        // This flips the sense of inequalities used in finding best moves and pruning searches.
        val mm = if (minMax.currentPlayer(game) == searchingPlayer) 1 else -1

        @tailrec
        def searchMoves(moves: Seq[G], best: Option[Eval[G]]): Int = moves match {
          case Nil =>
            // We should have selected a best move by now.
            best.map(_.eval).getOrElse(MinMaxError(s"No moves!"))
          case m :: ms =>
            val eval = searchPruned(m, prune = best.map(_.eval), depth = depth - 1)
            if (prune.exists(mm * _ < mm * eval)) {
              listener.prune(mm, prune.get, eval)
              eval
            } else {
              val b = selectBest(mm, best, Eval(m, eval))
              searchMoves(ms, b)
            }
        }

        minMax.moves(game) match {
          case Right(moves) =>
            searchMoves(moves.toList, None)
          case Left(gameOver) =>
            listener.evaluate(evaluator(game, searchingPlayer))
        }
      }
    }

    def searchTop(moves: NonEmptyList[G]): Eval[G] = {
      @tailrec def searchMoves(best: Option[Eval[G]], moves: List[G]): Option[Eval[G]] =
        moves match {
          case Nil =>
            best
          case head :: tail =>
            // prune with the current best value.
            val eval = searchPruned(head, prune = best.map(_.eval), depth = maxDepth - 1)
            // always maximizing at the top.
            val updatedBest = selectBest(1, best, Eval(head, eval))
            searchMoves(updatedBest, tail)
        }
      searchMoves(Option.empty[Eval[G]], moves.toList).getOrElse(MinMaxError(s"No moves!"))
    }

    minMax.moves(game).map(searchTop)
  }
}
