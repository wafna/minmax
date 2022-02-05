package wafna.games
package onitama

import cats.data.NonEmptyList
import wafna.games.minmax.*
import wafna.games.minmax.Arena.*
import wafna.games.Player.{P1, P2}
import wafna.games.minmax.MinMax.Eval

import scala.util.Random

object OnitamaMinMax {

  implicit val onitamaMinMax: MinMax[Onitama] = new MinMax[Onitama] {

    override def currentPlayer(game: Onitama): Player = if (game.pass.isRight) P1 else P2

    override def evaluate(game: Onitama, player: Player): Int = game.gameOver match {
      case Some(Draw) =>
        // Even if the player cannot move a piece the player must still exchange a card.
        sys.error("Draw disallowed.")
      case Some(Win(p)) => if (p == player) Int.MaxValue else Int.MinValue
      case None =>
        // todo worth favoring some early game optimizations?
        //   more interestingly, should evaluate be supplied by each player?
        0
    }

    override def moves(game: Onitama): Either[GameOver, NonEmptyList[Onitama]] = game.gameOver match {
      case None =>
        Right(game.moves())
      case Some(gameOver) =>
        Left(gameOver)
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val random: Random = scala.util.Random
    val (result, games): (GameOver, List[Onitama]) = Arena.runGame(Onitama(), new SearchBot[Onitama](4), new SearchBot[Onitama](4))
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
