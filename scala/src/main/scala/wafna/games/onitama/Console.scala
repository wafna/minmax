package wafna.games
package onitama

import wafna.games.minmax.MinMax
import wafna.games.minmax.MinMax.{MeterSnapshot, TimerSnapshot}

import java.text.NumberFormat
import scala.collection.immutable.ArraySeq

object Console {

  trait ShowLines[A] {
    def apply(a: A): List[String]
  }

  implicit class ShowMe[T : ShowLines](t: T) {
    def show(implicit showLines: ShowLines[T]): List[String] = showLines(t)
  }

  implicit class FormatLines(lines: List[String]) {
    def indent(spaces: Int)(lines: List[String]): List[String] = {
      require(0 <= spaces)
      val pad = " " * spaces
      lines.map(pad + _)
    }
    def block: String =
      lines.mkString(System.lineSeparator())
  }

  implicit val cardShowLines: ShowLines[Card] = (card: Card) => {
    val grid = Array.fill(25)(' ')
    grid(12) = 'O'
    card.moves.toList.foreach { move =>
      grid(move.x + 2 + (5 * (move.y + 2))) = 'X'
    }
    val lines = grid.grouped(5).toList.reverse.map(_.foldLeft("")(_ + _))
    card.name :: lines
  }

  implicit val handShowLines: ShowLines[List[Card]] = (cards: List[Card]) => {
    cards
      .map(_.show)
      .foldLeft(List.fill(6)("")) { (s, c) =>
        s.zip(c).map(p => p._1 ++ "%1$-9s".format(p._2))
      }
  }

  implicit val boardShowLines: ShowLines[ArraySeq[Option[Piece]]] = (board: ArraySeq[Option[Piece]]) => {
    val grid = Array.fill(25)(' ')
    board.zipWithIndex.foreach { case (s, i) =>
      s match {
        case None =>
          val spot = Spot.fromIx(i)
          if (spot.x == 2 && (spot.y == 0 || spot.y == 4)) {
            grid(i) = '-'
          } else {
            grid(i) = '.'
          }
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

  implicit val gameShowLines: ShowLines[Onitama] = (game: Onitama) => List(
    s"P1: ${game.p1.toNel.map(_.name).toList.mkString(", ")}",
    s"P2: ${game.p2.toNel.map(_.name).toList.mkString(", ")}",
    s"Turn: ${game.turnInHand.player} ${game.turnInHand.card.name}",
    s"State: ${game.gameOver}"
  ) ++ game.board.spots.show

  val numberFormat: NumberFormat = NumberFormat.getInstance()
  def formatNumber(n: Long): String = numberFormat.format(n)
  def showMeterSnapshot(snapshot: MeterSnapshot): String = {
    f"${formatNumber(snapshot.count)} (${snapshot.meanRate}%.0f)"
  }
  def showTimerSnapshot(snapshot: TimerSnapshot): String =
    f"${formatNumber(snapshot.count)} (${snapshot.average})"
  def showStats(stats: MinMax.Stats): String =
    s"""searches = ${showMeterSnapshot(stats.searches)}, evaluations = ${showTimerSnapshot(stats.evaluations)}
       |  prunes = ${formatNumber(stats.prunes)}, wins = ${formatNumber(stats.evalWins)}, losses = ${formatNumber(
      stats.evalLosses
    )}""".stripMargin

  def allCards: String = Deck.cards.toList.show.block
}
