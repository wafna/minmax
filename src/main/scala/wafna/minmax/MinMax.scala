package wafna.minmax

import wafna.minmax.MinMax.State
import wafna.minmax.MinMax.State.{Drawn, Open}
import wafna.util.Player

import scala.annotation.tailrec

/** Type class of MinMax games. */
trait MinMax[G] {
  def currentPlayer(game: G): Player
  def evaluate(game: G, player: Player): Int
  def moves(game: G): Seq[G]
  def pass(game: G): G
  def state(game: G): State
}

object MinMax {

  sealed trait State
  object State {
    final case object Open extends State
    final case object Drawn extends State
    final case class Won(player: Player) extends State
  }

  /** A game plus its valuation relative to the searching player. */
  final case class Eval[G](game: G, eval: Int)

  private def selectBest[G](mm: Int, best: Option[Eval[G]], maybe: Eval[G]): Option[Eval[G]] =
    if (best.exists(mm * _.eval > mm * maybe.eval)) {
      best
    } else {
      Some(maybe)
    }

  /** Search the current game to the specified depth for the best move.
    * @return None if there are no moves, perhaps there's a winner?
    */
  def search[G](game: G, maxDepth: Int)(implicit minMax: MinMax[G]): Option[Eval[G]] = {
    require(0 < maxDepth, "maxDepth must be positive.")
    // This player is the player initiating the search, throughout.
    val searchingPlayer = minMax.currentPlayer(game)
    minMax
      .moves(game)
      .foldLeft(Option.empty[Eval[G]]) { (best, move) =>
        // prune with the current best value.
        val eval: Int = evaluate(game = move, searchingPlayer: Player, prune = best.map(_.eval), depth = maxDepth - 1)
        // always maximizing at the top.
        selectBest(1, best, Eval(move, eval))
      }
  }
  private def evaluate[G](game: G, searchingPlayer: Player, prune: Option[Int], depth: Int)(implicit
    minMax: MinMax[G]
  ): Int = {
    if (0 == depth) {
      minMax.evaluate(game, searchingPlayer)
    } else {
      // This flips the sense of inequalities used in finding best moves and pruning searches.
      val mm = if (minMax.currentPlayer(game) == searchingPlayer) 1 else -1

      @tailrec
      def searchMoves(moves: Seq[G], best: Option[Eval[G]]): Int = moves match {
        case Nil =>
          best.map(_.eval).getOrElse {
            // If we have no moves and best is empty then we never had any moves.
            // So, the moving player passes.
            val pass: G = minMax.pass(game)
            evaluate(pass, searchingPlayer, best.map(_.eval), depth - 1)
          }
        case m :: ms =>
          val eval = evaluate(m, searchingPlayer, best.map(_.eval), depth - 1)
          if (prune.exists(p => mm * p < mm * eval)) {
            eval
          } else {
            val b = selectBest(mm, best, Eval(m, eval))
            searchMoves(ms, b)
          }
      }
      if (Open == minMax.state(game)) {
        searchMoves(minMax.moves(game), None)
      } else {
        minMax.evaluate(game, searchingPlayer)
      }
    }
  }
}
