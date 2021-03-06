package wafna.games
package onitama

import cats.data.NonEmptyList
import com.codahale.metrics.Snapshot
import wafna.games.minmax.*
import wafna.games.minmax.Arena.*
import wafna.games.minmax.MinMax.{Eval, MeterSnapshot, TimerSnapshot}
import wafna.games.onitama.Console.*

import java.text.NumberFormat
import scala.util.Random

object OnitamaMinMax {

  implicit val onitamaMinMax: MinMax[Onitama] = new MinMax[Onitama] {

    override def currentPlayer(game: Onitama): Player = game.currentPlayer

    override def moves(game: Onitama): Either[GameOver, NonEmptyList[Onitama]] = game.gameOver match {
      case None =>
        Right(game.moves())
      case Some(gameOver) =>
        Left(gameOver)
    }
  }

  abstract class OBot[L <: MinMax.Listener[Onitama]](depth: Int)(implicit
    listener: L = new MinMax.ListenerNoOp[Onitama]
  ) extends Arena.SearchBot[Onitama, L](depth) {

    def evaluateSafe(game: Onitama, player: Player): Int

    final override def evaluate(game: Onitama, player: Player): Int = game.gameOver match {
      case Some(Draw) =>
        // Even if the player cannot move a piece the player must still exchange a card.
        sys.error("Draw disallowed.")

      case Some(Win(p)) =>
        if (p == player) Int.MaxValue else Int.MinValue

      case None =>
        evaluateSafe(game, player)
    }
  }

  class OBot0[L <: MinMax.Listener[Onitama]](depth: Int)(implicit listener: L = new MinMax.ListenerNoOp[Onitama])
      extends OBot[L](depth) {
    override def evaluateSafe(game: Onitama, player: Player): Int = 0
  }
  object OBot0 {
    def apply[L <: MinMax.Listener[Onitama]](depth: Int, listener: L = new MinMax.ListenerNoOp[Onitama]): OBot0[L] = {
      implicit val q: L = listener
      new OBot0(depth)
    }
  }

  class OBot1[L <: MinMax.Listener[Onitama]](depth: Int)(implicit listener: L = new MinMax.ListenerNoOp[Onitama])
      extends OBot[L](depth) {

    //noinspection ScalaStyle
    override def evaluateSafe(game: Onitama, player: Player): Int = {

      // Attempting to favor positions of general advancement down field.
      val s1 = game.board.occupied(player)
      s1.foldLeft(0) { (p, s) =>
        val n = player match {
          case P1 => s.x
          case P2 => 4 - s.x
        }
        p + (n match {
          case 0 => 0
          case 1 => 8
          case 2 => 4
          case 3 => 2
          case 4 => 1
          case _ => sys.error("This is bad.")
        })
      }
    }
  }
  object OBot1 {
    def apply[L <: MinMax.Listener[Onitama]](depth: Int, listener: L = new MinMax.ListenerNoOp[Onitama]): OBot1[L] = {
      implicit val q: L = listener
      new OBot1(depth)
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val random: Random = scala.util.Random

    val p1 = OBot0(6, new MinMax.ListenerCounter("p1"))
    val p2 = OBot0(6, new MinMax.ListenerCounter("p2"))

    implicit val gameListener: Arena.GameListener[Onitama] = new GameListener[Onitama] {
      override def move(games: List[Onitama]): Unit = {
        println("----------------------")
        println(s"--- Turn ${games.size}")
        println(games.head.show.block)
      }
    }
    val (result, games): (GameOver, List[Onitama]) = Arena.runGame(Onitama(), p1, p2)
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(result)
    val g = games.head
    println(g.show.block)
    println("----------------------")
    println(g.cards.show.block)
    println("----------------------")
    println(s"P1: ${Console.showStats(p1.getListener.stats())}")
    println(s"P2: ${Console.showStats(p2.getListener.stats())}")
  }
}
