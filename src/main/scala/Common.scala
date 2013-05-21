package org.furidamu.SpaceMarauders

import akka.actor._

object Config {
  import com.typesafe.config.ConfigFactory
  val customConf = ConfigFactory.parseString("""
    akka {
      # Event handlers to register at boot time (Logging$DefaultLogger logs to STDOUT)
      # event-handlers = ["org.furidamu.SpaceMarauders.CustomLogger"]
      # Options: OFF, ERROR, WARNING, INFO, DEBUG
      loglevel = "DEBUG"
      scheduler {
        tick-duration = 10ms
      }
    }  """)

  val system = ActorSystem("MySystem", customConf)
  val WIDTH = 800
  val HEIGHT = 800

}

object Helper {
  def dump[T](value: T): T = {
    println(value)
    value
  }
}

class CustomLogger extends Actor {
  import akka.event.Logging._
  def receive = {
    case InitializeLogger(_)                        ⇒ sender ! LoggerInitialized
    case Error(cause, logSource, logClass, message) ⇒
      println(s"[ERROR] $cause - $message @ $logClass, $logSource")
    case Warning(logSource, logClass, message)      ⇒
      println(s"[WARN] $message")
    case Info(logSource, logClass, message)         ⇒
      println(s"[INFO] $message")
    case Debug(logSource, logClass, message)        ⇒
      println(s"[DEBUG] $message")
  }
}

class InputLogger extends Actor with ActorLogging {
  def receive = {
    case ButtonDown(ctrl, btn) => log.info(s"controller $ctrl pressed $btn")
    case ButtonUp(ctrl, btn) => log.info(s"controller $ctrl released $btn")
    case KeyDown(code, c) => log.info(s"key '$c' pressed ($code)")
    case KeyUp(code, c) => log.info(s"key '$c' released ($code)")
  }
}
