package org.furidamu.SpaceMarauders

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.BasicGame;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.InputListener
import org.newdawn.slick.ControllerListener
import org.newdawn.slick.command._

import scala.sys
import akka.actor._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout

case object ReadAxis

// X = Left/Right, Y = Up/Down
case class Axis(leftStickX: Float, leftStickY: Float,
  rightStickX: Float, rightStickY: Float, leftTrigger: Float, rightTrigger: Float)


object PadButton extends Enumeration {
  type PadButton = Value
  val A, B, X, Y, Back, Start, LeftBumper, RightBumper, PadLeft, PadRight, PadUp,
    PadDown, Guide, LeftStick, RightStick, Unknown = Value
}

import PadButton.PadButton

abstract class ButtonEvent
case class ButtonDown(controller: Int, btn: PadButton) extends ButtonEvent
case class ButtonUp(controller: Int, btn: PadButton) extends ButtonEvent

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
  val HEIGHT = 600
}


class InputActor extends Actor with ActorLogging with ControllerListener with InputProviderListener {
  var provider: InputProvider = _
  var input: Input = _
  val exit = new BasicCommand("exit")
  var controllers: Seq[Int] = (0 to 8)

  def receive = {
    case in: Input =>
      input = in
      input.addControllerListener(this)
      log.info("start listening for input")
      provider = new InputProvider(input);
      provider.addListener(this);
      provider.bindCommand(new KeyControl(Input.KEY_ESCAPE), exit)

      controllers = (0 until input.getControllerCount()).filter(c => input.getAxisCount(c) > 10)

      sender ! "done"

    case ReadAxis =>
      input.poll(Config.WIDTH, Config.HEIGHT)
      val controllerAxes = controllers.map {
        controller =>
          val axes = (0 until input.getAxisCount(controller)).map {
            a => input.getAxisValue(controller, a)
          } ++ List(0.0f, 0.0f, 0.0f, 0.0f)
          controller -> Axis(axes(5), axes(6), axes(8), axes(9), axes(7), axes(10))
      }
      sender ! controllerAxes.toMap
  }


  // Members declared in org.newdawn.slick.ControlledInputReciever
  var oldPressed: Seq[(Int, Seq[Float])] = (0 until 8).map(c => c -> (0 until 5).map(a => 0.0f))
  def inputEnded() = {
    val newPressed = controllers.map {
      controller => controller -> (0 until 5).map {
        axis => input.getAxisValue(controller, axis)
      }
    }

    for( ((cNum, controllerNew), (_, controllerOld)) <- newPressed.zip(oldPressed)) {
      for( ((axisNew, axisOld), aNum) <- controllerNew.zip(controllerOld).zipWithIndex) {
        if(axisNew != axisOld) {
          val btn = aNum match {
            case 0 => PadButton.Start
            case 1 => PadButton.PadLeft
            case 2 => PadButton.PadRight
            case 3 => PadButton.PadUp
            case 4 => PadButton.PadDown
          }

          val ev = if(axisNew == 1.0) ButtonDown(cNum, btn) else ButtonUp(cNum, btn)
          Config.system.eventStream.publish(ev)
        }
      }
    }

    oldPressed = newPressed
  }
  def inputStarted() = { }
  def isAcceptingInput(): Boolean = true
  def setInput(in: Input) = log.info("setInput")

  def translateButton(button: Int) = button match {
    case 1 => PadButton.A
    case 2 => PadButton.B
    case 3 => PadButton.X
    case 4 => PadButton.Y
    case 5 => PadButton.LeftBumper
    case 6 => PadButton.RightBumper
    case 7 => PadButton.Back
    case 8 => PadButton.Guide
    case 9 => PadButton.LeftStick
    case 10 => PadButton.RightStick
    case _ => PadButton.Unknown
  }

  // Members declared in org.newdawn.slick.ControllerListener
  def controllerButtonPressed(controller: Int, button: Int) {
    if(controllers.contains(controller)) {
      Config.system.eventStream.publish(ButtonDown(controller, translateButton(button)))
    }
  }

  def controllerButtonReleased(controller: Int, button: Int) {
    if(controllers.contains(controller)) {
      Config.system.eventStream.publish(ButtonUp(controller, translateButton(button)))
    }
  }
  def controllerDownPressed(controller: Int) {}
  def controllerDownReleased(controller: Int) {}
  def controllerLeftPressed(controller: Int) {}
  def controllerLeftReleased(controller: Int) {}
  def controllerRightPressed(controller: Int) {}
  def controllerRightReleased(controller: Int) {}
  def controllerUpPressed(controller: Int) {}
  def controllerUpReleased(controller: Int) {}


  def controlPressed(command: Command) {
    log.info("Pressed: "+ command)
  }

  def controlReleased(command: Command) {
    command match {
      case `exit` => sys.exit(0)
      case _ => log.info("Released: " + command)
    }
  }
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

  import scala.concurrent.duration._
  implicit val timeout = Timeout(1 seconds)

  def init(container: GameContainer) {
    Await.result(inputHandler ? container.getInput(), timeout.duration)
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
      println(s"[ERROR] $cause - $message")
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
  val rumbler = Config.system.actorOf(Props[Rumbler], name = "rumbler")

  def receive = {
    case "start" =>
      gameLogic = new InputProviderTest()
      mainThread ! gameLogic

    case ButtonDown(controller, btn) =>
      if(!gameLogic.controllersToDraw.contains(controller)) {
        gameLogic.controllersToDraw = (controller :: gameLogic.controllersToDraw).sorted
      }

      btn match {
        case PadButton.A => rumbler ! Rumble(controller, (gameLogic.axes(controller).rightTrigger + 1) / 2)
        case PadButton.B => rumbler ! Rumble(controller, 0.0f)
        case PadButton.Y => rumbler ! "info"
        case _ =>
      }
  }
}


case class Rumble(controller: Int, strength: Float)

class Rumbler extends Actor with ActorLogging {
  import net.java.games.input.Controller.Type;
  import net.java.games.input.ControllerEnvironment;

  def receive = {
    case Rumble(ctrl, strength) =>
      val cEnvironment = ControllerEnvironment.getDefaultEnvironment();
        for(rumbler <- cEnvironment.getControllers()(ctrl - 4).getRumblers()) {
          log.info(s"rumbling @ $strength");
          rumbler.rumble(strength);
        }
  }
}


import java.io.File
import org.lwjgl.LWJGLUtil

object Game extends App {
  System.setProperty("org.lwjgl.librarypath", new File(new File(new File(System.getProperty("user.dir"), "lib"), "native"), LWJGLUtil.getPlatformName()).getAbsolutePath());
  System.setProperty("net.java.games.input.librarypath", System.getProperty("org.lwjgl.librarypath"));

  // val rumbler = new RumbleTest()

  val game = Config.system.actorOf(Props[GameActor], name = "game")
  Config.system.eventStream.subscribe(game, classOf[ButtonEvent])
  game ! "start"
}
