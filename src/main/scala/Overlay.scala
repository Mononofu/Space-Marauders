package org.furidamu.SpaceMarauders

import java.awt._; //Graphics2D, LinearGradientPaint, Point, Window, Window.Type;
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

        def getCircleButtons(): (Int, Boolean, Boolean) = {
          var leftTrigger = false
          var rightTrigger = false
          for((i, pad) <- axes) {
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

        import GraphicConversions._
        val (highlighted, leftTrigger, rightTrigger) = getCircleButtons()
        circleInput.render(highlighted, leftTrigger, rightTrigger, CanDrawAwt(g))
    }
  }

  class InputActor
}

case object Repaint

class ExitActor extends Actor with ActorLogging {
  val gamepadActor = context.actorFor("/user/gamepadActor")
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
      log.info("try")
      ev match {
        case KeyDown(Key.ESCAPE, c) =>
          window.map {
            case w =>
              log.info("closing")
              w.setVisible(false);
              w.dispose()
              component.map(_.quit())
              window = None
          }
        case ButtonDown(ctrl, PadButton.Back) => sys.exit(0)
        case ButtonDown(ctrl, PadButton.A) =>
          if(window.isEmpty)
            createWindow()
        case _ => next.head ! EventLink(ev, next.tail)
      }
    case Repaint =>
      component.map(_.repaint(0, 0, 0, 800, 800))
  }
}


class XInputActor extends Actor {
  import scala.sys.process._

  def receive = {
    case KeyUp(code, c) => code match {
      case Key.SPACE => "xdotool key space" !
      case Key.BACKSPACE => "xdotool key BackSpace" !
      case Key.LEFT => "xdotool key Left" !
      case Key.RIGHT => "xdotool key Right" !
      case Key.UP => "xdotool key Up" !
      case Key.DOWN => "xdotool key Down" !
      case Key.RETURN => "xdotool key Return" !
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
  }
}



import java.io.File
import org.lwjgl.LWJGLUtil
import org.newdawn.slick.Input
import Config.system.dispatcher

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
  Config.system.scheduler.schedule(0 milliseconds,
      30 milliseconds,
      exitActor,
      Repaint)
  inputActor ! SubscribeAll(inputLogger)
  inputActor ! SubscribeUnhandled(exitActor)
  inputActor ! SubscribeAll(xInputActor)


}
