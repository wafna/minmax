package wafna

import cats.data.NonEmptyList

package object util {

  @inline def nonEmptyList[T](head: T, tail: T*): NonEmptyList[T] = NonEmptyList(head, tail.toList)

  sealed trait Player {
    def opponent: Player
  }
  object Player {
    case object P1 extends Player {
      def opponent: Player = P2
    }
    case object P2 extends Player {
      def opponent: Player = P1
    }
  }
}
