package wafna.games.minmax

import cats.data.NonEmptyList
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
    def search(): Unit
    def prune(): Unit
    def evaluate(): Unit
  }
  object ListenerNoOp extends Listener {
    override def search(): Unit = ()
    override def prune(): Unit = ()
    override def evaluate(): Unit = ()
  }

  case class Stats(searches: Int, prunes: Int, evaluations: Int)

  class ListenerCounter extends Listener {
    private var searches = 0
    private var prunes = 0
    private var evaluations = 0
    override def search(): Unit = searches += 1
    override def prune(): Unit = prunes += 1
    override def evaluate(): Unit = evaluations += 1
    def stats(): Stats = Stats(searches, prunes, evaluations)
  }

  /** A game plus its valuation relative to the searching player. */
  final case class Eval[G](game: G, eval: Int)

  @inline private def selectBest[G](mm: Int, best: Option[Eval[G]], maybe: Eval[G]): Option[Eval[G]] =
    if (best.exists(mm * _.eval > mm * maybe.eval)) {
      best
    } else {
      Some(maybe)
    }

  type Evaluator[G] = (G, Player) => Int

  /** Search the current game to the specified depth for the best move.
    */
  def search[G](game: G, maxDepth: Int, evaluator: Evaluator[G])(implicit
    minMax: MinMax[G], listener: Listener = ListenerNoOp
  ): Either[GameOver, Eval[G]] = {
    require(0 < maxDepth, "maxDepth must be positive.")
    // This player is the player initiating the search, throughout.
    val searchingPlayer = minMax.currentPlayer(game)
    minMax
      .moves(game)
      .map {
        _.foldLeft(Option.empty[Eval[G]]) { (best, move) =>
          // prune with the current best value.
          val eval: Int = searchPruned(move, searchingPlayer, evaluator, prune = best.map(_.eval), depth = maxDepth - 1)
          // always maximizing at the top.
          selectBest(1, best, Eval(move, eval))
        }.getOrElse(MinMaxError(s"No moves!"))
      }
  }

  /** Below the first layer of search we may have a pruning value which we use to abort unfruitful searches.
    */
  private def searchPruned[G](
    game: G,
    searchingPlayer: Player,
    evaluator: Evaluator[G],
    prune: Option[Int],
    depth: Int
  )(implicit minMax: MinMax[G], listener: Listener): Int = {
    if (0 == depth) {
      listener.evaluate()
      evaluator(game, searchingPlayer)
    } else {
      listener.search()
      // This flips the sense of inequalities used in finding best moves and pruning searches.
      val mm = if (minMax.currentPlayer(game) == searchingPlayer) 1 else -1

      @tailrec
      def searchMoves(moves: Seq[G], best: Option[Eval[G]]): Int = moves match {
        case Nil =>
          // We should have selected a best move by now.
          best.map(_.eval).getOrElse(MinMaxError(s"No moves!"))
        case m :: ms =>
          val eval = searchPruned(m, searchingPlayer, evaluator, prune = best.map(_.eval), depth = depth - 1)
          if (prune.exists(p => mm * p < mm * eval)) {
            listener.prune()
            eval
          } else {
            val b = selectBest(mm, best, Eval(m, eval))
            searchMoves(ms, b)
          }
      }

      minMax.moves(game) match {
        case Left(Draw)        => 0
        case Left(Win(winner)) => if (searchingPlayer == winner) Int.MaxValue else Int.MinValue
        case Right(moves) =>
          searchMoves(moves.toList, None)
      }
    }
  }
}
