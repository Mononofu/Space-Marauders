package org.furidamu.SpaceMarauders

import java.awt.{Graphics2D, LinearGradientPaint, Point, Window, Graphics, Color, Font, Image}
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.ImageIO;
import java.io.IOException;

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.duration._

import Helper._
import Config.system.dispatcher

case class CanDrawAwt(g: Graphics) extends CanDraw {
  def setColor(c: org.newdawn.slick.Color) { g.setColor(new Color(c.r, c.g, c.b)) }
  def fillOval(x: Float, y: Float, width: Float, height: Float) {
    g.fillOval(x.toInt, y.toInt, width.toInt, height.toInt)
  }
  def fillPolygon(xPoints: Seq[Float], yPoints: Seq[Float]) {
    g.fillPolygon(xPoints.map(_.toInt).toArray, yPoints.map(_.toInt).toArray, xPoints.length)
  }

  def drawString(font: CanWrite, x: Float, y: Float, str: String, c: org.newdawn.slick.Color) =
    font.drawString(this, x, y, str, c)
  def getHeight(font: CanWrite, str: String) = font.getHeight(this, str)
  def getWidth(font: CanWrite, str: String) = font.getWidth(this, str)

  def drawImage(img: ImageLike, x: Float, y: Float) = img.draw(this, x, y)
}

case class CanWriteAwt(f: Font) extends CanWrite {
    def drawString[T <: CanDraw](g: T, x: Float, y: Float, str: String,
      c: org.newdawn.slick.Color) = g match {
      case g: CanDrawAwt =>
        g.g.setFont(f)
        g.setColor(c)
        g.g.drawString(str, x.toInt, y.toInt + getAscent(g, str))
    }
    def getHeight[T <: CanDraw](g: T, str: String) = g match {
      case g: CanDrawAwt =>
        g.g match {
          case g: Graphics2D =>
            f.getStringBounds(str, g.getFontRenderContext()).getHeight().toInt
        }
    }
    def getWidth[T <: CanDraw](g: T, str: String) = g match {
      case g: CanDrawAwt =>
        g.g match {
          case g: Graphics2D =>
            f.getStringBounds(str, g.getFontRenderContext()).getWidth().toInt
        }
    }

    private def getAscent(g: CanDrawAwt, str: String) = g.g match {
      case g: Graphics2D =>
        f.getLineMetrics(str, g.getFontRenderContext()).getAscent().toInt
    }
}

case class ImageLikeAwt(img: Image) extends ImageLike {
  def draw[T <: CanDraw](g: T, x: Float, y: Float) = g match {
    case g: CanDrawAwt => // g.g.drawImage(img, x.toInt, y.toInt, new Color(0, 0, 0, 255), null)
  }
  def getWidth() = img.getWidth(null)
  def getHeight() = img.getHeight(null)
}


class AboutComponent(gamepadActor: ActorRef) extends JPanel {
  val myFont = Font.createFont(Font.TRUETYPE_FONT,
    new File("res/fonts/DroidSansMonoDotted.ttf"))
  val myImg = ImageIO.read(new File("res/img/left_stick.png"));
  val circleInput = new CircleInput[CanDrawAwt, CanWriteAwt, ImageLikeAwt](CanWriteAwt(
    myFont.deriveFont(Font.PLAIN, 20f)), ImageLikeAwt(myImg))

  implicit val timeout = Timeout(20 milliseconds)

  def quit() = circleInput.quit()

  override def paintComponent(graphics: Graphics) {
    graphics match {
      case g: Graphics2D =>
        val axisFuture = gamepadActor ? ReadAxis
        val axes = Await.result(axisFuture, timeout.duration).asInstanceOf[Map[Int, Axis]]

        def getHighlighted(): Int = {
          for((i, pad) <- axes) {
            if(Math.abs(pad.leftStickX) > 0.2 || Math.abs(pad.leftStickY) > 0.2) {
              val phi = Math.atan2(pad.leftStickX, pad.leftStickY)
              return (12 - (phi * 8 / (2*Math.PI) + 0.5).toInt) % 8
            }
          }
          return -1
        }

        import GraphicConversions._
        circleInput.render(getHighlighted(), CanDrawAwt(g))
    }
  }

  class InputActor
}

case object Repaint
case object UpdateMouseMove

class ExitActor extends Actor with ActorLogging {
  implicit val timeout = Timeout(20 milliseconds)

  val gamepadActor = context.actorFor("/user/gamepadActor")
  val xInputActor = context.actorFor("/user/xInputActor")
  var component: Option[AboutComponent] = None
  var window: Option[JFrame] = None

  def createWindow() = {
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        val frame = new JFrame("Circle Overlay");
        window = Some(frame)
        //turn of window decorations
        frame.setFocusableWindowState(false);
        frame.setUndecorated(true);
        //turn off the background
        frame.setBackground(new Color(0, 0, 0, 0));
        component = Some(new AboutComponent(gamepadActor))
        frame.setContentPane(component.get);
        frame.pack();
        //size the window
        frame.setSize(800, 800);
        frame.setVisible(true);
        //center on screen
        frame.setLocationRelativeTo(null);
      }
    } );
  }

  def receive = {
    case EventLink(ev, next) =>
      ev match {
        case KeyDown(Key.ESCAPE, _) | KeyDown(Key.RETURN, _) =>
          window.map {
            case w =>
              w.setVisible(false);
              w.dispose()
              component.map(_.quit())
              component = None
              window = None
          }
        case ButtonDown(ctrl, PadButton.Back) => sys.exit(0)
        case ButtonDown(ctrl, PadButton.A) =>
          if(window.isEmpty) {
            createWindow()
          }
        case ButtonDown(ctrl, PadButton.LeftTrigger) =>
          xInputActor ! ExecuteLines(List("keydown Super_L", "keydown Left",
            "keyup Super_L", "keyup Left"))
        case ButtonDown(ctrl, PadButton.RightTrigger) =>
          xInputActor ! ExecuteLines(List("keydown Super_L", "keydown Right",
            "keyup Super_L", "keyup Right"))
        case ButtonDown(ctrl, PadButton.RightBumper) =>
          xInputActor ! ExecuteLines(List("keydown Control_L", "keydown Tab",
            "keyup Control_L", "keyup Tab"))
        case ButtonDown(ctrl, PadButton.LeftBumper) =>
          xInputActor ! ExecuteLines(List("keydown Control_L", "keydown Shift_L",
            "keydown Tab", "keyup Shift_L", "keyup Control_L", "keyup Tab"))
        case ButtonDown(ctrl, PadButton.LeftStick) =>
          xInputActor ! ExecuteLines(List("mouseclick 2"))
        case ButtonDown(ctrl, PadButton.RightStick) =>
          xInputActor ! ExecuteLines(List("mouseclick 1"))
        case ButtonDown(ctrl, PadButton.PadRight) =>
          xInputActor ! ExecuteLines(List("keydown Control_L", "keydown plus",
            "keyup plus", "keyup Control_L"))
        case ButtonDown(ctrl, PadButton.PadLeft) =>
          xInputActor ! ExecuteLines(List("keydown Control_L", "keydown minus",
            "keyup minus", "keyup Control_L"))
        case ButtonDown(ctrl, PadButton.PadUp) =>
          xInputActor ! ExecuteLines(List("key Up"))
        case ButtonDown(ctrl, PadButton.PadDown) =>
          xInputActor ! ExecuteLines(List("key Down"))
        case AxisMoved(ctrl, axis, newValue) =>
          if(component.isEmpty) {
            rescheduleScrolling()
          }
          component.map(_.repaint(0, 0, 0, 800, 800))
        case _ => next.head ! EventLink(ev, next.tail)
      }
    case UpdateMouseMove =>
      xInputActor ! moveMessage
  }

  var scrollTask: Option[Cancellable] = None
  var moveTask: Option[Cancellable] = None
  var moveMessage = ExecuteLines(List())

  def rescheduleScrolling() = {
    val axisFuture = gamepadActor ? ReadAxis
    val axes = Await.result(axisFuture, timeout.duration).asInstanceOf[Map[Int, Axis]]

    def getScrollInterval(): (Int, ExecuteLines) = {
      for((i, pad) <- axes) {
        if(pad.leftStickY > 0.2) {
          return  ((30 / Math.pow(pad.leftStickY, 3)).toInt, ExecuteLines(List("mouseclick 5")))
        } else if(pad.leftStickY < -0.2) {
          return  ((30 / Math.pow(pad.leftStickY.abs, 3)).toInt, ExecuteLines(List("mouseclick 4")))
        }
      }
      return (-1, null)
    }

    def getMouseMoveDistance(): Option[(Int, Int)] = {
      for((i, pad) <- axes) {
        if(pad.rightStickX.abs > 0.2 || pad.rightStickY > 0.2) {
          return Some((20 * Math.pow(pad.rightStickX, 3)).toInt, (20 * Math.pow(pad.rightStickY, 3)).toInt)
        }
      }
      return None
    }

    scrollTask.map(_.cancel())
    scrollTask = None

    val (interval, action) = getScrollInterval()

    if(interval > 0) {
      println(s"$interval ms: $action")
      scrollTask = Some(Config.system.scheduler.schedule(0 milliseconds,
          interval milliseconds,
          xInputActor,
          action))
    }


    val moveDistance = getMouseMoveDistance()
    if(moveDistance.isEmpty) {
      moveTask.map(_.cancel())
      moveTask = None
    } else {
      val (dx, dy) = moveDistance.get
      moveMessage = ExecuteLines(List(s"mousermove $dx $dy"))
      if(moveTask.isEmpty) {
        moveTask = Some(Config.system.scheduler.schedule(0 milliseconds,
          10 milliseconds,
          self,
          UpdateMouseMove))
      }
    }
  }
}

case class ExecuteLines(l: Seq[String])

class XInputActor extends Actor {
  import scala.sys.process._

  val pb = new java.lang.ProcessBuilder("xte")
  val p = pb.start()
  val pw = new java.io.PrintWriter(new java.io.OutputStreamWriter(p.getOutputStream()), true)

  def receive = {
    case KeyUp(code, c) => code match {
      case Key.SPACE => pw.println("key space")
      case Key.BACKSPACE => pw.println("key BackSpace")
      case Key.LEFT => pw.println("key Left")
      case Key.RIGHT => pw.println("key Right")
      case Key.UP => pw.println("key Up")
      case Key.DOWN => pw.println("key Down")
      case Key.RETURN => pw.println("key Return")
      case _ => c match {
        case '.' => "xdotool key period" !
        case ',' => "xdotool key comma" !
        case ':' => "xdotool key colon" !
        case '/' => "xdotool key slash" !
        case '@' => "xdotool key at" !
        case '-' => "xdotool key minus" !
        case _ => s"xdotool key $c" !
      }
    }
    case ExecuteLines(l) =>
      l.foreach(pw.println)
  }
}



import java.io.File
import org.lwjgl.LWJGLUtil
import org.newdawn.slick.Input

object Overlay extends App {
  System.setProperty("org.lwjgl.librarypath", new File(new File(new File(System.getProperty("user.dir"), "lib"), "native"), LWJGLUtil.getPlatformName()).getAbsolutePath());
  System.setProperty("net.java.games.input.librarypath", System.getProperty("org.lwjgl.librarypath"));

  // very important - use lwjgl for gamepad input!
  Input.disableControllers()

  val inputActor = Config.system.actorOf(Props[InputActor], name = "inputActor")
  val gamepadActor = Config.system.actorOf(Props[GamepadActor], name = "gamepadActor")
  val inputLogger = Config.system.actorOf(Props[InputLogger], name = "inputLogger")
  val exitActor = Config.system.actorOf(Props[ExitActor], name = "exitActor")
  val xInputActor = Config.system.actorOf(Props[XInputActor], name = "xInputActor")

  Config.system.scheduler.schedule(0 milliseconds,
      10 milliseconds,
      gamepadActor,
      Poll)
  inputActor ! SubscribeAll(inputLogger)
  inputActor ! SubscribeUnhandled(exitActor)
  inputActor ! SubscribeAll(xInputActor)


}
