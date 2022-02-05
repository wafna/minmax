package wafna.games.minmax

import cats.data.NonEmptyList
import nl.grons.metrics4.scala.{DefaultInstrumented, Meter}
import wafna.games.Player

import scala.annotation.tailrec

/** Type class of MinMax games. */
trait MinMax[G] {

  /** The player with turn in hand for the game.
    */
  def currentPlayer(game: G): Player

  /** Return a non-empty list of moves from a game state
    * or declare the game to be over.
    */
  def moves(game: G): Either[GameOver, NonEmptyList[G]]
}

sealed trait GameOver
case object Draw extends GameOver
case class Win(player: Player) extends GameOver

object MinMax {

  class MinMaxError(msg: String) extends Exception(msg)

  object MinMaxError {
    def apply(msg: String): Nothing = throw new MinMaxError(msg)
  }

  trait Listener {

    def search(depth: Int): Unit
    def prune(): Unit
    def evaluate(eval: Int): Unit
  }

  object ListenerNoOp extends Listener {

    override def search(depth: Int): Unit = ()
    override def prune(): Unit = ()
    override def evaluate(eval: Int): Unit = ()
  }

  case class MeterSnapshot(count: Long, meanRate: Double)

  object MeterSnapshot {
    def apply(meter: Meter): MeterSnapshot =
      MeterSnapshot(meter.count, meter.meanRate)
  }

  case class Stats(searches: MeterSnapshot, prunes: MeterSnapshot, evaluations: MeterSnapshot)

  class ListenerCounter(name: String) extends Listener with DefaultInstrumented {
    private val searches = metrics.meter(s"searches-$name")
    private val prunes = metrics.meter(s"prunes-$name")
    private val evaluations = metrics.meter(s"evaluations-$name")

    override def search(depth: Int): Unit = searches.mark()
    override def prune(): Unit = prunes.mark()
    override def evaluate(eval: Int): Unit = evaluations.mark()

    def stats(): Stats = Stats(MeterSnapshot(searches), MeterSnapshot(prunes), MeterSnapshot(evaluations))
  }

  /** A game plus its valuation relative to the searching player. */
  final case class Eval[G](game: G, eval: Int)

  type Evaluator[G] = (G, Player) => Int

  /** Search the current game to the specified depth for the best move.
    */
  //noinspection ScalaStyle
  def search[G](game: G, maxDepth: Int, evaluator: Evaluator[G])(implicit
    minMax: MinMax[G],
    listener: Listener = ListenerNoOp
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
    def searchPruned[G](game: G, evaluator: Evaluator[G], prune: Option[Int], depth: Int)(implicit
      minMax: MinMax[G],
      listener: Listener
    ): Int = {

      if (0 == depth) {
        val eval = evaluator(game, searchingPlayer)
        listener.evaluate(eval)
        eval
      } else {
        listener.search(maxDepth - depth)
        // This flips the sense of inequalities used in finding best moves and pruning searches.
        val mm = if (minMax.currentPlayer(game) == searchingPlayer) 1 else -1

        @tailrec
        def searchMoves(moves: Seq[G], best: Option[Eval[G]]): Int = moves match {
          case Nil =>
            // We should have selected a best move by now.
            best.map(_.eval).getOrElse(MinMaxError(s"No moves!"))
          case m :: ms =>
            val eval = searchPruned(m, evaluator, prune = best.map(_.eval), depth = depth - 1)
            if (prune.exists(p => mm * p < mm * eval)) {
              listener.prune()
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
            val eval = evaluator(game, searchingPlayer)
            listener.evaluate(eval)
            eval
        }
      }
    }

    minMax
      .moves(game)
      .map {
        _.foldLeft(Option.empty[Eval[G]]) { (best, move) =>
          // prune with the current best value.
          val eval: Int = searchPruned(move, evaluator, prune = best.map(_.eval), depth = maxDepth - 1)
          // always maximizing at the top.
          selectBest(1, best, Eval(move, eval))
        }.getOrElse(MinMaxError(s"No moves!"))
      }

  }
}
