package org.furidamu.SpaceMarauders


import scala.sys
import akka.actor._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.BasicGame;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.InputListener
import org.newdawn.slick.ControllerListener
import org.newdawn.slick.command._


object Config {
  import com.typesafe.config.ConfigFactory
  val customConf = ConfigFactory.parseString("""
    akka {
      # Event handlers to register at boot time (Logging$DefaultLogger logs to STDOUT)
      event-handlers = ["org.furidamu.SpaceMarauders.CustomLogger"]
      # Options: OFF, ERROR, WARNING, INFO, DEBUG
      loglevel = "DEBUG"
    }  """)

  val system = ActorSystem("MySystem", ConfigFactory.load(customConf))
  val WIDTH = 800
  val HEIGHT = 300
}



class InputLogger extends Actor with ActorLogging {
  def receive = {
    case ButtonDown(ctrl, btn) => log.info(s"controller $ctrl pressed $btn")
    case ButtonUp(ctrl, btn) => log.info(s"controller $ctrl released $btn")
  }
}



class InputProviderTest extends BasicGame("InputProvider Test") {
  val attack = new BasicCommand("attack");
  val jump = new BasicCommand("jump");
  val run = new BasicCommand("run");
  var message = "";
  val inputHandler = Config.system.actorOf(Props[InputActor], name = "inputHandler")
  val inputLogger = Config.system.actorOf(Props[InputLogger], name = "inputLogger")
  Config.system.eventStream.subscribe(inputLogger, classOf[ButtonEvent])

  import Config.system.dispatcher
  import scala.concurrent.duration._
  implicit val timeout = Timeout(1 seconds)

  Config.system.scheduler.schedule(0 milliseconds,
      10 milliseconds,
      inputHandler,
      Poll)

  def init(container: GameContainer) {
    Await.result(inputHandler ? "start", timeout.duration)
  }

  var controllersToDraw = List[Int]()

  def render(container: GameContainer, g: Graphics) {
    g.drawString("Press any button to register a controller", 10, 50);
    g.drawString(message,100,150);



    var offset = 0
    for(c <- controllersToDraw) {
      val pad = axes(c)

      g.drawOval(80, 80 + offset, 100, 100)
      g.drawOval(125 + pad.leftStickX * 50, 125 + pad.leftStickY * 50 + offset, 10, 10)

      g.drawOval(230, 80 + offset, 100, 100)
      g.drawOval(275 + pad.rightStickX * 50, 125 + pad.rightStickY * 50 + offset, 10, 10)

      g.drawRect(380, 80 + offset, 20, 100)
      g.drawOval(385, 125 + pad.leftTrigger * 50 + offset, 10, 10)

      g.drawRect(430, 80 + offset, 20, 100)
      g.drawOval(435, 125 + pad.rightTrigger * 50 + offset, 10, 10)

      g.drawString(s"controller $c", 470, 120 + offset)

      offset += 130
    }

  }

  var axes = Map[Int, Axis]()
  def update(container: GameContainer, delta: Int) {
    val axisFuture = inputHandler ? ReadAxis
    axes = Await.result(axisFuture, timeout.duration).asInstanceOf[Map[Int, Axis]]
  }
}

class MainThread extends Actor {
  def receive = {
    case gameLogic: InputProviderTest =>
      val container = new AppGameContainer(gameLogic)
      container.setDisplayMode(Config.WIDTH, Config.HEIGHT, false)
      container.start()
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


class GameActor extends Actor {
  var gameLogic: InputProviderTest = _
  val mainThread = Config.system.actorOf(Props[MainThread], name = "mainActor")

  def receive = {
    case "start" =>
      Input.disableControllers()
      gameLogic = new InputProviderTest()
      mainThread ! gameLogic

    case ButtonDown(controller, btn) =>
      println("here")
      if(!gameLogic.controllersToDraw.contains(controller)) {
        println("adding " + controller)
        gameLogic.controllersToDraw = (controller :: gameLogic.controllersToDraw).sorted
      }

      btn match {
        case PadButton.A => gameLogic.inputHandler ! Rumble(controller, (gameLogic.axes(controller).rightTrigger + 1) / 2)
        case PadButton.B => gameLogic.inputHandler ! Rumble(controller, 0.0f)
        case PadButton.Back => sys.exit(0)
        case _ =>
      }
  }
}


import java.io.File
import org.lwjgl.LWJGLUtil

object MyGame extends App {
  System.setProperty("org.lwjgl.librarypath", new File(new File(new File(System.getProperty("user.dir"), "lib"), "native"), LWJGLUtil.getPlatformName()).getAbsolutePath());
  System.setProperty("net.java.games.input.librarypath", System.getProperty("org.lwjgl.librarypath"));

  //val rumbler = new RumbleTest()

  val game = Config.system.actorOf(Props[GameActor], name = "game")
  Config.system.eventStream.subscribe(game, classOf[ButtonEvent])
  game ! "start"
}
