package wafna.minmax

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/** Type class of MinMax games.
  * @tparam G The type of the game.
  * @tparam P The type of the player of the game.
  */
trait MinMax[G, P] {
  def currentPlayer(game: G): P
  def maximize(game: G, player: P): Boolean
  def evaluate(game: G, player: P): Int
  def moves(game: G): Seq[G]
  def pass(game: G): G
  def show(game: G): String
}

object MinMax {
  private val log = LoggerFactory.getLogger(getClass)
  final case class Eval[G](game: G, eval: Int)
  private def selectBest[G](mm: Int, best: Option[Eval[G]], maybe: Eval[G]): Option[Eval[G]] =
    if (best.exists(b => mm * b.eval > mm * maybe.eval)) {
      best
    } else {
      Some(maybe)
    }

  /** Search the current game to the specified depth for the best move.
    * @return None if there are no moves, perhaps there's a winner?
    */
  def search[G, P](game: G, maxDepth: Int)(implicit minMax: MinMax[G, P]): Option[Eval[G]] = {
    require(0 < maxDepth, "maxDepth must be positive.")
    // This player is the player initiating the search, throughout.
    val currentPlayer = minMax.currentPlayer(game)
    minMax
      .moves(game)
      .foldLeft(Option.empty[Eval[G]]) { (best, move) =>
        // prune with the current best value.
        val eval: Int = evaluate(game = move, currentPlayer: P, prune = best.map(_.eval), depth = maxDepth - 1)
        // always maximizing at the top.
        selectBest(1, best, Eval(move, eval))
      }
  }
  private def evaluate[G, P](game: G, currentPlayer: P, prune: Option[Int], depth: Int)(implicit
    minMax: MinMax[G, P]
  ): Int = {
    if (0 == depth) {
      val eval = minMax.evaluate(game, currentPlayer)
      log.debug(s"evaluate [$depth] eval=$eval\n${minMax.show(game)}")
      eval
    } else {
      val mm = if (minMax.maximize(game, currentPlayer)) 1 else -1
      log.debug(s"search [$depth] prune=${prune.getOrElse(" ")}\n${minMax.show(game)}")
      @tailrec
      def searchMoves(moves: Seq[G], best: Option[Eval[G]]): Int = moves match {
        case Nil =>
          best.map(_.eval).getOrElse {
            // If we have no moves and best is empty then we never had any moves.
            // So, the moving player passes.
            val pass: G = minMax.pass(game)
            val eval: Int = evaluate(pass, currentPlayer, best.map(_.eval), depth - 1)
            log.debug(s"PASS depth=$depth, eval=$eval\n${minMax.show(pass)}")
            eval
          }
        case m :: ms =>
          val eval = evaluate(m, currentPlayer, best.map(_.eval), depth - 1)
          log.debug(s"move [$depth] eval=$eval, best=${best.map(_.eval)}\n${minMax.show(m)}")
          if (prune.exists(p => mm * p < mm * eval)) {
            log.debug(s"PRUNED [$depth] prune=${prune.get}, eval=$eval\n${minMax.show(m)}")
            eval
          } else {
            val b = selectBest(mm, best, Eval(m, eval))
            searchMoves(ms, b)
          }
      }
      searchMoves(minMax.moves(game), None)
    }
  }
}
