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

  class OBot0(depth: Int) extends SearchBot[Onitama](depth) {

    //noinspection ScalaStyle
    override def evaluate(game: Onitama, player: Player): Int = game.gameOver match {

      case Some(Draw) =>
        // Even if the player cannot move a piece the player must still exchange a card.
        sys.error("Draw disallowed.")

      case Some(Win(p)) =>
        if (p == player) Int.MaxValue else Int.MinValue

      case None =>
//        // Dislike having King threatened.
//        if (game.currentPlayer != player) {
//          val kingSpot = game.board.spots.indexOf(Some(Piece(player, King)))
//        } else {
//          // todo worth favoring some early game optimizations?
//          0
//        }
        val s1 = game.board.occupied(player)
        s1.foldLeft(0) { (p, s) =>
          val n = player match {
            case P1 => s.x
            case P2 => 4 - s.x
          }
          n match {
            case 0 => 0
            case 1 => 8
            case 2 => 4
            case 3 => 2
            case 4 => 1
            case _ => sys.error("This is bad.")
          }
        }
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val random: Random = scala.util.Random
    val (result, games): (GameOver, List[Onitama]) = Arena.runGame(Onitama(), new OBot0(4), new OBot0(4))
    println(result)
    val turns = games.size
    games.zipWithIndex.foreach { case (game, turn) =>
      println(s"-- Turn ${turns - turn}")
      println(Console.show(game).mkString("\n"))
    }
    val g = games.head
    val cards: List[Card] = g.p1.toNel.toList ++ g.p2.toNel.toList ++ List(g.pass.getOrElse(g.pass.swap.toOption.get))
    println("----------------------")
    cards.foreach(card => println(Console.show(card).mkString("\n")))
  }
}
