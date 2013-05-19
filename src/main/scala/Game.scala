package org.furidamu.SpaceMarauders


import scala.sys
import akka.actor._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout

import org.newdawn.slick._
import org.newdawn.slick.command._
import org.newdawn.slick.font.effects.{ColorEffect, Effect}
import org.newdawn.slick.geom.{Rectangle, Transform}

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
  inputHandler ! SubscribeAll(inputLogger)

  import Config.system.dispatcher
  import scala.concurrent.duration._
  implicit val timeout = Timeout(1 seconds)

  var font: UnicodeFont = _;


  Config.system.scheduler.schedule(0 milliseconds,
      10 milliseconds,
      inputHandler,
      Poll)

  def init(container: GameContainer) {
    Await.result(inputHandler ? Start, timeout.duration)

    font = new UnicodeFont("res/fonts/DroidSansMonoDotted.ttf", 20, true, false);
    font.addAsciiGlyphs();
    font.getEffects() match {
      case effects: java.util.List[Effect] => effects.add(new ColorEffect());
    }
      // Create a default white color effect
    font.loadGlyphs();
  }

  var controllersToDraw = List[Int]()

  def render(container: GameContainer, g: Graphics) {
    g.setColor(Color.white)
    g.drawString("Press any button to register a controller", 10, 50);
    g.drawString(message,100,150);

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


    def getHighlighted(): Int = {
      for(c <- controllersToDraw) {
        val pad = axes(c)
        if(Math.abs(pad.leftStickX) > 0.1 || Math.abs(pad.leftStickY) > 0.1) {
          val phi = Math.atan2(pad.leftStickX, pad.leftStickY)
          return (12 - (phi * 8 / (2*Math.PI) + 0.5).toInt) % 8
        }
      }
      return -1
    }

    val hightlightedCircle = getHighlighted()

    val darkYellow = new Color(179, 142, 31)
    val darkBlue = new Color(25, 66, 127)
    val darkRed = new Color(165, 28, 12)
    val darkGreen = new Color(107, 144, 12)
    val circleHighlighted = new Color(56, 91, 112)
    val circleNormal = new Color(33, 57, 71)
    val darkBackgroundCircle = new Color(28, 49, 61)
    val transparent = List(Color.transparent, Color.transparent, Color.transparent, Color.transparent)
    val colors = List(darkBlue, darkYellow, darkRed, darkGreen)
    val o = 40
    val offsets = List( (-o, 0), (0, -o), (o, 0), (0, o) )

    val chars = ('a' to 'z').map(_.toString) ++ List(",", ".", ":", "/", "@", "-")
    val charGroupCircleRadius = 70
    val charGroupCircleOffset = 185
    val backgroundCircleRadius = charGroupCircleOffset + charGroupCircleRadius +
      charGroupCircleRadius / 2 - 5

    g.setColor(darkBackgroundCircle)
    g.fillOval(80, 80, 2*backgroundCircleRadius, 2*backgroundCircleRadius)

    val middleCircleRadius = charGroupCircleOffset
    g.setColor(circleNormal)
    g.fillOval(80 + backgroundCircleRadius - middleCircleRadius,
      80 + backgroundCircleRadius - middleCircleRadius, 2*middleCircleRadius,
      2* middleCircleRadius)

    for((charGroup, i) <- chars.grouped(4).zipWithIndex) {
      val xOffset = charGroupCircleOffset * Math.cos(2*Math.PI * ((i+6) % 8) / 8)
      val yOffset = charGroupCircleOffset * Math.sin(2*Math.PI * ((i+6) % 8) / 8)
      val posX = backgroundCircleRadius - charGroupCircleRadius + xOffset.toInt + 80
      val posY = backgroundCircleRadius - charGroupCircleRadius + yOffset.toInt + 80

      if(i == hightlightedCircle) {
        drawCharsCircle(charGroup, colors, offsets, posX, posY,
          charGroupCircleRadius, circleHighlighted, i, g)
      } else {
        drawCharsCircle(charGroup, transparent, offsets, posX, posY,
          charGroupCircleRadius, circleNormal, i, g)
      }
    }
  }

  def drawCharsCircle(chars: Seq[String], colors: Seq[Color], offsets: Seq[(Int, Int)],
    x: Int, y: Int, r: Int, circleColor: Color, index: Int, g: Graphics) {
    g.setColor(circleColor)
    g.fillOval(x, y, 2*r, 2*r)

    val rect = (new Rectangle(0, 0, r, r)).transform(
      Transform.createRotateTransform((Math.PI * (index-3)/4.).toFloat)).transform(
      Transform.createTranslateTransform(x+r, y+r))

    g.fill(rect)

    val childRadius = 20
    chars.zip(colors).zip(offsets).foreach {
      case ((letter, color), offset) =>
        drawCharCircle(letter, color, x + r + offset._1 - childRadius,
          y + r + offset._2 - childRadius, childRadius, g)
    }
  }

  def drawCharCircle(c: String, circleColor: Color, x: Int, y: Int, r: Int, g: Graphics) {
    g.setColor(circleColor)
    g.fillOval(x, y, r*2, r*2)

    font.drawString(x + r - font.getWidth(c) /2 - 1,
      y + r - font.getHeight(c)/2 - 1, c, Color.white)
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
    case Start =>
      // very important - use lwjgl for gamepad input!
      Input.disableControllers()
      gameLogic = new InputProviderTest()
      gameLogic.inputHandler ! SubscribeUnhandled(self)
      mainThread ! gameLogic

    case EventLink(ev @ ButtonDown(controller, btn), next) =>
      if(!gameLogic.controllersToDraw.contains(controller)) {
        gameLogic.controllersToDraw = (controller :: gameLogic.controllersToDraw).sorted
      }

      btn match {
        case PadButton.A => gameLogic.inputHandler ! Rumble(controller, (gameLogic.axes(controller).rightTrigger + 1) / 2)
        case PadButton.B => gameLogic.inputHandler ! Rumble(controller, 0.0f)
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

  //val rumbler = new RumbleTest()

  val game = Config.system.actorOf(Props[GameActor], name = "game")
  Config.system.eventStream.subscribe(game, classOf[ButtonEvent])
  game ! Start
}
