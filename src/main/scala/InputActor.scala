package org.furidamu.SpaceMarauders

import org.lwjgl.input.Controllers;
import akka.actor._

abstract class InputEvent

object Key extends Enumeration {
  type Key = Value
  val A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y,
    Z, SPACE, BACKSPACE, DELETE, RETURN, SHIFT, CONTROL, _0, _1, _2, _3, _4, _5,
    _6, _7, _8, _9, LEFT, RIGHT, UP, DOWN, ESCAPE, UNKNOWN = Value
  def fromChar(c: Char) = c match {
    case 'a' | 'A' => A
    case 'b' | 'B' => B
    case 'c' | 'C' => C
    case 'd' | 'D' => D
    case 'e' | 'E' => E
    case 'f' | 'F' => F
    case 'g' | 'G' => G
    case 'h' | 'H' => H
    case 'i' | 'I' => I
    case 'j' | 'J' => J
    case 'k' | 'K' => K
    case 'l' | 'L' => L
    case 'm' | 'M' => M
    case 'n' | 'N' => N
    case 'o' | 'O' => O
    case 'p' | 'P' => P
    case 'q' | 'Q' => Q
    case 'r' | 'R' => R
    case 's' | 'S' => S
    case 't' | 'T' => T
    case 'u' | 'U' => U
    case 'v' | 'V' => V
    case 'w' | 'W' => W
    case 'x' | 'X' => X
    case 'y' | 'Y' => Y
    case 'z' | 'Z' => Z
    case '0' => _0
    case '1' => _1
    case '2' => _2
    case '3' => _3
    case '4' => _4
    case '5' => _5
    case '6' => _6
    case '7' => _7
    case '8' => _8
    case '9' => _9
    case _ => UNKNOWN
  }
}
import Key.Key

case class KeyDown(code: Key, c: Char) extends InputEvent
case class KeyUp(code: Key, c: Char) extends InputEvent

case class SubscribeAll(actor: ActorRef)
case class SubscribeUnhandled(actor: ActorRef)
case class Unsubscribe(actor: ActorRef)
case class EventLink(e: InputEvent, next: Seq[ActorRef])

class UnhandledEventActor extends Actor with ActorLogging {
  def receive = {
    case EventLink(e, Nil) => log.info(s"unhandled: $e")
    case EventLink(e, next) => log.warning("I should be the last actor in the chain")
  }
}


class InputActor extends Actor with ActorLogging {
  val unhandledActor = Config.system.actorOf(Props[UnhandledEventActor], name = "unhandledActor")
  var subscribers = List[ActorRef]()
  var unhandled_subscribers = List[ActorRef](unhandledActor)

  def receive = {
    case SubscribeAll(sub) =>
      subscribers = sub :: subscribers

    case SubscribeUnhandled(sub) =>
      unhandled_subscribers = sub :: unhandled_subscribers

    case Unsubscribe(sub) =>
      subscribers = subscribers.filter(_ != sub)
      unhandled_subscribers = unhandled_subscribers.filter(_ != sub)

    // receives any input event and rebroadcasts it to all subscribers
    case ev: InputEvent =>
      for(sub <- subscribers) {
        sub ! ev
      }
      unhandled_subscribers.head ! EventLink(ev, unhandled_subscribers.tail)
  }
}
