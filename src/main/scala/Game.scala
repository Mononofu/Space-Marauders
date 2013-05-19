package org.furidamu.SpaceMarauders

import scala.sys
import akka.actor._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import akka.testkit.CallingThreadDispatcher

import Config.system.dispatcher
import scala.concurrent.duration._

import org.newdawn.slick._
import org.newdawn.slick.command._
import org.newdawn.slick.font.effects.{ColorEffect, Effect}
import org.newdawn.slick.geom.{Rectangle, Transform}


class InputLogger extends Actor with ActorLogging {
  def receive = {
    case ButtonDown(ctrl, btn) => log.info(s"controller $ctrl pressed $btn")
    case ButtonUp(ctrl, btn) => log.info(s"controller $ctrl released $btn")
    case KeyDown(code, c) => log.info(s"key '$c' pressed ($code)")
    case KeyUp(code, c) => log.info(s"key '$c' released ($code)")
  }
}

class InputProviderTest(gamepadActor: ActorRef) extends BasicGame("InputProvider Test") {
  var circleInput = Config.system.actorOf(Props[CircleInput].withDispatcher(CallingThreadDispatcher.Id), name = "circleInput")
  implicit val timeout = Timeout(20 milliseconds)

  def init(container: GameContainer) {
    circleInput ! Start
  }

  var controllersToDraw = List[Int]()

  def render(container: GameContainer, g: Graphics) {
    g.setColor(Color.white)
    g.drawString("Press any button to register a controller", 10, 50);

    var offset = 0
    for(c <- controllersToDraw) {
      val pad = axes(c)

      g.drawOval(80, 80 + offset, 100, 100)
      g.fillOval(125 + pad.leftStickX * 50, 125 + pad.leftStickY * 50 + offset, 10, 10)

      g.drawOval(230, 80 + offset, 100, 100)
      g.fillOval(275 + pad.rightStickX * 50, 125 + pad.rightStickY * 50 + offset, 10, 10)

      g.drawRect(380, 80 + offset, 20, 100)
      g.fillOval(385, 125 + pad.leftTrigger * 50 + offset, 10, 10)

      g.drawRect(430, 80 + offset, 20, 100)
      g.fillOval(435, 125 + pad.rightTrigger * 50 + offset, 10, 10)

      g.drawString(s"controller $c", 470, 120 + offset)

      offset += 130
    }


    def getCircleButtons(): (Int, Boolean, Boolean) = {
      var leftTrigger = false
      var rightTrigger = false
      for(c <- controllersToDraw) {
        val pad = axes(c)
        if(!leftTrigger)
          leftTrigger = pad.leftTrigger > -0.6
        if(!rightTrigger)
          rightTrigger = pad.rightTrigger > -0.6

        if(Math.abs(pad.leftStickX) > 0.2 || Math.abs(pad.leftStickY) > 0.2) {
          val phi = Math.atan2(pad.leftStickX, pad.leftStickY)
          val highlighted = (12 - (phi * 8 / (2*Math.PI) + 0.5).toInt) % 8
          return (highlighted, leftTrigger, rightTrigger)
        }
      }
      return (-1, leftTrigger, rightTrigger)
    }

    val (highlighted, leftTrigger, rightTrigger) = getCircleButtons()
    circleInput ! RenderCircle(highlighted, leftTrigger, rightTrigger, g)
  }

  var axes = Map[Int, Axis]()
  def update(container: GameContainer, delta: Int) {
    val axisFuture = gamepadActor ? ReadAxis
    axes = Await.result(axisFuture, timeout.duration).asInstanceOf[Map[Int, Axis]]
  }
}


class GameActor extends Actor {
  val gamepadActor = context.actorFor("/user/gamepadActor")
  val gameLogic = new InputProviderTest(gamepadActor)
  val mainThread = new Thread(new Runnable {
    def run() {
      val container = new AppGameContainer(gameLogic)
      container.setDisplayMode(Config.WIDTH, Config.HEIGHT, false)
      container.start()
    }
  })

  def receive = {
    case Start =>
      mainThread.start()

    case EventLink(ev @ ButtonDown(controller, btn), next) =>
      if(!gameLogic.controllersToDraw.contains(controller)) {
        gameLogic.controllersToDraw = (controller :: gameLogic.controllersToDraw).sorted
      }

      btn match {
        case PadButton.A => gamepadActor ! Rumble(controller, (gameLogic.axes(controller).rightTrigger + 1) / 2)
        case PadButton.B => gamepadActor ! Rumble(controller, 0.0f)
        case PadButton.Back => sys.exit(0)
        case _ => next.head ! EventLink(ev, next.tail)
      }
  }
}


import java.io.File
import org.lwjgl.LWJGLUtil

object MyGame extends App {
  System.setProperty("org.lwjgl.librarypath", new File(new File(new File(System.getProperty("user.dir"), "lib"), "native"), LWJGLUtil.getPlatformName()).getAbsolutePath());
  System.setProperty("net.java.games.input.librarypath", System.getProperty("org.lwjgl.librarypath"));

  // very important - use lwjgl for gamepad input!
  Input.disableControllers()

  val inputActor = Config.system.actorOf(Props[InputActor], name = "inputActor")
  val gamepadActor = Config.system.actorOf(Props[GamepadActor], name = "gamepadActor")
  val inputLogger = Config.system.actorOf(Props[InputLogger], name = "inputLogger")
  val game = Config.system.actorOf(Props[GameActor], name = "game")

  Config.system.scheduler.schedule(0 milliseconds,
      10 milliseconds,
      gamepadActor,
      Poll)
  inputActor ! SubscribeAll(inputLogger)
  inputActor ! SubscribeUnhandled(game)

  game ! Start

  Thread.sleep(10000)
}
