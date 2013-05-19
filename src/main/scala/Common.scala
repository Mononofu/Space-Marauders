package org.furidamu.SpaceMarauders

import akka.actor._

object Config {
  import com.typesafe.config.ConfigFactory
  val customConf = ConfigFactory.parseString("""
    akka {
      # Event handlers to register at boot time (Logging$DefaultLogger logs to STDOUT)
      event-handlers = ["org.furidamu.SpaceMarauders.CustomLogger"]
      # Options: OFF, ERROR, WARNING, INFO, DEBUG
      loglevel = "DEBUG"
    }  """)

  val system = ActorSystem("MySystem")
  val WIDTH = 800
  val HEIGHT = 300
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
