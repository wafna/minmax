package wafna.hexapawn

import wafna.util.Player
import wafna.util.Player.{P1, P2}

class Hexapawn private[hexapawn] (
  val currentPlayer: Player,
  val cols: Int,
  val rows: Int,
  val state: Hexapawn.DrawCond,
  spots: Hexapawn.Grid
) {
  import Hexapawn._

  @inline private def fromGrid(col: Int, row: Int): Int = col * rows + row
  def spot(col: Int, row: Int): Option[Player] = spots(fromGrid(col, row))

  private case class Params(goal: Int, delta: Int, moveBand: (Int, Int))
  private def params(p: Player): Params = p match {
    case P1 => Params(rows - 1, 1, (0, rows - 2))
    case P2 => Params(0, -1, (1, rows - 1))
  }

  lazy val winner: Option[Option[Player]] = {
    if (state == DrawCond.Drawn) {
      Some(None)
    } else {
      def winCon(player: Player): Option[Option[Player]] = {
        val goal = params(player).goal
        if ((0 until cols).exists(col => spot(col, goal).contains(player))) {
          Some(Some(player))
        } else {
          None
        }
      }
      // we assume the opponent just moved so we look for that player's win con, first, or not at all.
      winCon(currentPlayer.opponent) //.orElse(winCon(currentPlayer))
    }
  }

  def pass(): Hexapawn = {
    require(nextMoves.isEmpty)
    if (state == DrawCond.Passed) {
      new Hexapawn(currentPlayer, cols, rows, DrawCond.Drawn, spots)
    } else {
      new Hexapawn(currentPlayer.opponent, cols, rows, DrawCond.Passed, spots)
    }
  }
  lazy val nextMoves: List[Hexapawn] =
    if (winner.isDefined || state == DrawCond.Drawn) {
      Nil
    } else {
      val ps = params(currentPlayer)
      val opp = currentPlayer.opponent
      import ps._
      (0 until cols).foldLeft(List.empty[Hexapawn]) { (moves, col) =>
        (moveBand._1 to moveBand._2).foldLeft(moves) { (moves, row) =>
          if (spot(col, row).contains(currentPlayer)) {
            val grid = fromGrid(col, row)
            val forward: Option[Hexapawn] =
              if (spot(col, row + delta).isEmpty) {
                val newSpots = new Grid(spots.length)
                spots.copyToArray(newSpots)
                newSpots(fromGrid(col, row + delta)) = Some(currentPlayer)
                newSpots(grid) = None
                Some(new Hexapawn(opp, cols, rows, DrawCond.Open, newSpots))
              } else {
                None
              }
            val left: Option[Hexapawn] =
              if (col == 0) {
                None
              } else if (spot(col - 1, row + delta).contains(opp)) {
                val newSpots = new Grid(spots.length)
                spots.copyToArray(newSpots)
                newSpots(fromGrid(col - 1, row + delta)) = Some(currentPlayer)
                newSpots(grid) = None
                Some(new Hexapawn(opp, cols, rows, DrawCond.Open, newSpots))
              } else {
                None
              }
            val right: Option[Hexapawn] = {
              if (col == cols - 1) {
                None
              } else if (spot(col + 1, row + delta).contains(opp)) {
                val newSpots = new Array[Option[Player]](spots.length)
                spots.copyToArray(newSpots)
                newSpots(fromGrid(col + 1, row + delta)) = Some(currentPlayer)
                newSpots(grid) = None
                Some(new Hexapawn(opp, cols, rows, DrawCond.Open, newSpots))
              } else {
                None
              }
            }
            List(forward, left, right).flatten ++ moves
          } else {
            moves
          }
        }
      }
    }
  def show(): String = {
    val s = new StringBuilder()
    s.append(s"player = ${currentPlayer.show()}, state = $state, winner = $winner")
    s.append("\n")
    (0 until rows).foreach { urow =>
      val row = rows - urow - 1
      s.append(s"$row")
      (0 until cols).foreach { col =>
        s.append(" ")
        s.append(spot(col, row) match {
          case None    => " "
          case Some(p) => p.show()
        })
      }
      s.append("\n")
    }
    s.append(" ")
    s.append((0 until cols).map(" " + _).mkString(""))
    s.toString()
    s.toString()
  }
}

object Hexapawn {

  implicit class ShowPlayer(player: Player) {
    def show(): String = player match {
      case P1 => "X"
      case P2 => "O"
    }
  }

  sealed trait DrawCond
  object DrawCond {
    final case object Open extends DrawCond
    final case object Passed extends DrawCond
    final case object Drawn extends DrawCond
  }

  /** Creates the initial board with P1 and P2 filling the bottom (0) and top rows, respectively.
    */
  def apply(cols: Int, rows: Int): Hexapawn = {
    require(3 <= cols)
    require(3 <= rows)
    val spots: Array[Option[Player]] = Array.fill(cols * rows)(None)
    (0 until cols).foreach { col =>
      spots(col * rows) = Some(P1)
      spots((col + 1) * rows - 1) = Some(P2)
    }
    new Hexapawn(P1, cols, rows, DrawCond.Open, spots)
  }
  type Grid = Array[Option[Player]]
}
