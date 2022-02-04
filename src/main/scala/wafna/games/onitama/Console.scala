package wafna.games.onitama

import wafna.games.Player.{P1, P2}

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
      card.moves.foreach { move =>
        grid(move.x + 2 + (5 * (move.y + 2))) = 'X'
      }
      val lines = grid.grouped(5).toList.reverse.map(_.foldLeft("")(_ + _))
      card.name :: lines
    }
  }
  implicit val boardPrinter: Printer[Iterator[Option[Piece]]] = new Printer[Iterator[Option[Piece]]] {
    def getLines(board: Iterator[Option[Piece]]): List[String] = {
      val grid = Array.fill(25)(' ')
      board.zipWithIndex.foreach { case (s, i) =>
        s match {
          case None => // ignore
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

  def main(args: Array[String]): Unit = {
    println("Onimata")
    val g0 = Onitama()
    Deck.cards.foreach { card =>
      println(show(card).mkString("\n"))
    }
    println(show(g0.grid()).mkString("\n"))
  }
}
