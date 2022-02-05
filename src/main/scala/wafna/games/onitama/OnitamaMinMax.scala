package wafna.games
package onitama

import cats.data.NonEmptyList
import wafna.games.Player.{P1, P2}
import wafna.games.minmax.*
import wafna.games.minmax.Arena.*
import wafna.games.minmax.MinMax.Eval

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

  class OBot0[L <: MinMax.Listener](depth: Int)(implicit listener: L = MinMax.ListenerNoOp) extends SearchBot[Onitama, L](depth) {

    //noinspection ScalaStyle
    override def evaluate(game: Onitama, player: Player): Int = game.gameOver match {

      case Some(Draw) =>
        // Even if the player cannot move a piece the player must still exchange a card.
        sys.error("Draw disallowed.")

      case Some(Win(p)) =>
        if (p == player) Int.MaxValue else Int.MinValue

      case None =>
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
  object OBot0 {
    def apply[L <: MinMax.Listener](depth: Int, listener: L = MinMax.ListenerNoOp): OBot0[L] = {
      implicit val q: L = listener
      new OBot0(depth)
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val random: Random = scala.util.Random

    val p1 = OBot0(8, new MinMax.ListenerCounter)
    val p2 = OBot0(8, new MinMax.ListenerCounter)

    val (result, games): (GameOver, List[Onitama]) = Arena.runGame(Onitama(), p1, p2)
    println(result)
    val turns = games.size
    games.zipWithIndex.foreach { case (game, turn) =>
      println(s"--- Turn ${turns - turn}")
      println(Console.show(game).mkString("\n"))
    }
    val g = games.head
    val cards: List[Card] = g.p1.toNel.toList ++ g.p2.toNel.toList ++ List(g.pass.getOrElse(g.pass.swap.toOption.get))
    println("----------------------")
//    cards.foreach(card => println(Console.show(card).mkString("\n")))
    println(cards.map(Console.show).foldLeft(List.fill(6)("")) { (s, c) =>
      s.zip(c).map(p => p._1 ++ "%1$-9s".format(p._2))
    }.mkString("\n"))
    def showStats(stats: MinMax.Stats): String =
      s"searches = ${stats.searches}, evaluations = ${stats.evaluations}, prunes = ${stats.prunes}"
    println(s"P1: ${showStats(p1.getListener.stats())}")
    println(s"P2: ${showStats(p2.getListener.stats())}")
  }
}
