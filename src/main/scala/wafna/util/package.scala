package wafna

package object util {

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
