package wafna.games.minmax

import cats.data.NonEmptyList
import wafna.games.Player

import scala.annotation.tailrec

/** Type class of MinMax games. */
trait MinMax[G] {

  /** The player with turn in hand for the game.
    */
  def currentPlayer(game: G): Player

  /** An evaluation of a game state from the perspective of a player.
    */
  def evaluate(game: G, player: Player): Int

  /** Return a non-empty list of moves from a game state
    * or declare the game to be over.
    */
  def moves(game: G): Either[GameOver, NonEmptyList[G]]
}

sealed trait GameOver
case object Draw extends GameOver
case class Win(player: Player) extends GameOver

object MinMax {

  /** A game plus its valuation relative to the searching player. */
  final case class Eval[G](game: G, eval: Int)

  private def selectBest[G](mm: Int, best: Option[Eval[G]], maybe: Eval[G]): Option[Eval[G]] =
    if (best.exists(mm * _.eval > mm * maybe.eval)) {
      best
    } else {
      Some(maybe)
    }

  class MinMaxError(msg: String) extends Exception(msg)
  object MinMaxError {
    def apply(msg: String): Nothing = throw new MinMaxError(msg)
  }

  /** Search the current game to the specified depth for the best move.
    */
  def search[G](game: G, maxDepth: Int)(implicit minMax: MinMax[G]): Either[GameOver, Eval[G]] = {
    require(0 < maxDepth, "maxDepth must be positive.")
    // This player is the player initiating the search, throughout.
    val searchingPlayer = minMax.currentPlayer(game)
    minMax
      .moves(game)
      .map {
        _.foldLeft(Option.empty[Eval[G]]) { (best, move) =>
          // prune with the current best value.
          val eval: Int = searchPruned(game = move, searchingPlayer: Player, prune = best.map(_.eval), depth = maxDepth - 1)
          // always maximizing at the top.
          selectBest(1, best, Eval(move, eval))
        }.getOrElse(MinMaxError(s"No moves!"))
      }
  }
  /** Below the first layer of search we may have a pruning value which we use to abort unfruitful searches.
    */
  private def searchPruned[G](game: G, searchingPlayer: Player, prune: Option[Int], depth: Int)(implicit
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
          // We should have selected a best move by now.
          best.map(_.eval).getOrElse(MinMaxError(s"No moves!"))
        case m :: ms =>
          val eval = searchPruned(m, searchingPlayer, best.map(_.eval), depth - 1)
          if (prune.exists(p => mm * p < mm * eval)) {
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
