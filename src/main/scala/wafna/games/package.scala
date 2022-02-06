package wafna

import cats.data.NonEmptyList

package object games {

  @inline def nonEmptyList[T](head: T, tail: T*): NonEmptyList[T] =
    NonEmptyList(head, tail.toList)

  sealed trait Player {
    def opponent: Player
  }
  case object P1 extends Player {
    def opponent: Player = P2
  }
  case object P2 extends Player {
    def opponent: Player = P1
  }

  sealed trait GameOver
  case object Draw extends GameOver
  final case class Win(player: Player) extends GameOver
}
