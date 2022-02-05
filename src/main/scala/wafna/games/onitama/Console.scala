package wafna.games.onitama

import wafna.games.Player.{P1, P2}

import scala.collection.immutable.ArraySeq

//noinspection ConvertExpressionToSAM
object Console {

  trait Printer[A] {
    def getLines(a: A): List[String]
  }

  def show[A](a: A)(implicit printer: Printer[A]): List[String] = printer.getLines(a)

  implicit val cardPrinter: Printer[Card] = new Printer[Card] {

    def getLines(card: Card): List[String] = {
      val grid = Array.fill(25)(' ')
      grid(12) = 'O'
      card.moves.toList.foreach { move =>
        grid(move.x + 2 + (5 * (move.y + 2))) = 'X'
      }
      val lines = grid.grouped(5).toList.reverse.map(_.foldLeft("")(_ + _))
      card.name :: lines
    }
  }

  implicit val boardPrinter: Printer[ArraySeq[Option[Piece]]] = new Printer[ArraySeq[Option[Piece]]] {

    def getLines(board: ArraySeq[Option[Piece]]): List[String] = {
      val grid = Array.fill(25)(' ')
      board.zipWithIndex.foreach { case (s, i) =>
        s match {
          case None => // ignore
            grid(i) = '.'
          case Some(piece) =>
            piece match {
              case Piece(P1, Pawn) => grid(i) = 'x'
              case Piece(P1, King) => grid(i) = 'X'
              case Piece(P2, Pawn) => grid(i) = 'o'
              case Piece(P2, King) => grid(i) = 'O'
            }
        }
      }
      grid.grouped(5).toList.reverse.map(_.foldLeft("")(_ + _))
    }
  }

  implicit val gamePrinter: Printer[Onitama] = new Printer[Onitama] {

    def getLines(game: Onitama): List[String] =
      List(
        "P1: " ++ game.p1.toNel.map(_.name).toList.mkString(", "),
        "P2: " ++ game.p2.toNel.map(_.name).toList.mkString(", "),
        "Pass: " ++ (game.pass match {
          case Left(c) => s"P2 ${show(c).head}"
          case Right(c) => s"P1 ${show(c).head}"
        })
      ) ++ show(game.board.spots)
  }

  def main(args: Array[String]): Unit = {
    println("Onimata")
    val g0 = Onitama()
    Deck.cards.toList.foreach { card =>
      println(show(card).mkString("\n"))
    }
    println(show(g0.board.spots).mkString("\n"))
  }
}
