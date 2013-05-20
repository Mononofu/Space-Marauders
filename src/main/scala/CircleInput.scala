package org.furidamu.SpaceMarauders

import akka.actor._

import org.newdawn.slick._
import org.newdawn.slick.command._
import org.newdawn.slick.font.effects.{ColorEffect, Effect}
import org.newdawn.slick.geom.{Rectangle, Transform, Polygon}

trait CanDraw {
  def setColor(c: Color): Unit
  def fillOval(x1: Float, y: Float, width: Float, height: Float): Unit
  def fillPolygon(xPoints: Seq[Float], yPoints: Seq[Float]): Unit

  def drawString(font: CanWrite, x: Float, y: Float, str: String, c: Color): Unit
  def getHeight(font: CanWrite, str: String): Int
  def getWidth(font: CanWrite, str: String): Int
}

trait CanWrite {
  def drawString[T <: CanDraw](g: T, x: Float, y: Float, str: String, c: Color): Unit
  def getHeight[T <: CanDraw](g: T, str: String): Int
  def getWidth[T <: CanDraw](g: T, str: String): Int
}

import Helper._

object GraphicConversions {
  case class CanDrawSlick(g: Graphics) extends CanDraw {
    def setColor(c: Color) { g.setColor(c) }
    def fillOval(x: Float, y: Float, width: Float, height: Float) {
      g.fillOval(x, y, width, height)
    }
    def fillPolygon(xPoints: Seq[Float], yPoints: Seq[Float]) {
      g.fill(new Polygon(xPoints.zip(yPoints).map(p =>
        List(p._1, p._2)).flatten.toArray))
    }

    def drawString(font: CanWrite, x: Float, y: Float, str: String, c: Color) =
      font.drawString(this, x, y, str, c)
    def getHeight(font: CanWrite, str: String) = font.getHeight(this, str)
    def getWidth(font: CanWrite, str: String) = font.getWidth(this, str)
  }

  case class CanWriteSlick(f: Font) extends CanWrite {
    def drawString[T <: CanDraw](g: T, x: Float, y: Float, str: String,
      c: Color) = f.drawString(x, y, str, c)
    def getHeight[T <: CanDraw](g: T, str: String) = f.getHeight(str)
    def getWidth[T <: CanDraw](g: T, str: String) = f.getWidth(str)
  }
}

class CircleInput[G <: CanDraw, F <: CanWrite](font: F) {
  var circleInputActor = Config.system.actorOf(Props(new CircleInputActor), name = "circleInputActor")

  val darkYellow = new Color(179, 142, 31)
  val darkBlue = new Color(25, 66, 127)
  val darkRed = new Color(165, 28, 12)
  val darkGreen = new Color(107, 144, 12)
  val circleHighlighted = new Color(56, 91, 112)
  val circleNormal = new Color(33, 57, 71)
  val darkBackgroundCircle = new Color(28, 49, 61)
  val normal = List(circleNormal, circleNormal, circleNormal, circleNormal)
  val colors = List(darkBlue, darkYellow, darkRed, darkGreen)
  val o = 40
  val offsets = List( (-o, 0), (0, -o), (o, 0), (0, o) )

  val lowerChars = ('a' to 'z').map(_.toString) ++ List(",", ".", ":", "/", "@", "-")
  val upperChars = lowerChars.map(_.toUpperCase)
  val numberChars = (0 to 9).map(_.toString) ++ List("?", "!", "\"", "$", "€", "%",
    "&", "*", "(", ")", "+", ";", "_", "=", "[", "]", "{", "}", "<", ">", "'", "~")
  val specialChars = (0 to 32).map(n => " ")
  val charGroupCircleRadius = 70
  val charGroupCircleOffset = 185
  val backgroundCircleRadius = charGroupCircleOffset + charGroupCircleRadius +
    charGroupCircleRadius / 2 - 5

  var hightlightedCircle = -1
  var leftTrigger = false
  var rightTrigger = false
  var charsToDraw = lowerChars

  class CircleInputActor extends Actor {
    val inputActor = context.actorFor("/user/inputActor")
    inputActor ! SubscribeUnhandled(self)

    def receive = {
      case EventLink(ev @ ButtonDown(controller, btn), next) =>
        btn match {
          case PadButton.A | PadButton.B | PadButton.X | PadButton.Y =>
            if(hightlightedCircle >= 0) {
              val group = charsToDraw.grouped(4).toList(hightlightedCircle)
              var char = (btn match {
                case PadButton.X => group(0)
                case PadButton.Y => group(1)
                case PadButton.B => group(2)
                case PadButton.A => group(3)
              })(0)
              inputActor ! KeyDown(Key.fromChar(char), char)
              inputActor ! KeyUp(Key.fromChar(char), char)
            }
          case PadButton.RightBumper =>
            inputActor ! KeyDown(Key.SPACE, ' ')
            inputActor ! KeyUp(Key.SPACE, ' ')
          case PadButton.LeftBumper =>
            inputActor ! KeyDown(Key.BACKSPACE, 0)
            inputActor ! KeyUp(Key.BACKSPACE, 0)
          case _ => next.head ! EventLink(ev, next.tail)
        }
    }
  }


  def render(h: Int, l: Boolean, r: Boolean, g: G) {
    hightlightedCircle = h
    leftTrigger = l
    rightTrigger = r

    if(leftTrigger) {
      if(rightTrigger) {
        charsToDraw = specialChars
      } else {
        charsToDraw = upperChars
      }
    } else {
      if(rightTrigger) {
        charsToDraw = numberChars
      } else {
        charsToDraw = lowerChars
      }
    }
    g.setColor(darkBackgroundCircle)
    g.fillOval(80, 80, 2*backgroundCircleRadius, 2*backgroundCircleRadius)

    val middleCircleRadius = charGroupCircleOffset
    g.setColor(circleNormal)
    g.fillOval(80 + backgroundCircleRadius - middleCircleRadius,
      80 + backgroundCircleRadius - middleCircleRadius, 2*middleCircleRadius,
      2* middleCircleRadius)

    for((charGroup, i) <- charsToDraw.grouped(4).zipWithIndex) {
      val xOffset = charGroupCircleOffset * Math.cos(2*Math.PI * ((i+6) % 8) / 8)
      val yOffset = charGroupCircleOffset * Math.sin(2*Math.PI * ((i+6) % 8) / 8)
      val posX = backgroundCircleRadius - charGroupCircleRadius + xOffset.toInt + 80
      val posY = backgroundCircleRadius - charGroupCircleRadius + yOffset.toInt + 80

      if(i == hightlightedCircle) {
        drawCharsCircle(charGroup, colors, offsets, posX, posY,
          charGroupCircleRadius, circleHighlighted, i, g)
      } else {
        drawCharsCircle(charGroup, normal, offsets, posX, posY,
          charGroupCircleRadius, circleNormal, i, g)
      }
    }
  }

  def drawCharsCircle(chars: Seq[String], colors: Seq[Color],
    offsets: Seq[(Int, Int)], x: Int, y: Int, r: Int, circleColor: Color,
    index: Int, g: G) {
    g.setColor(circleColor)
    g.fillOval(x, y, 2*r, 2*r)

    val rect = (new Rectangle(0, 0, r, r)).transform(
      Transform.createRotateTransform((Math.PI * (index-3)/4.).toFloat)).transform(
      Transform.createTranslateTransform(x+r, y+r))
    val (xPoints, yPoints) = rect.getPoints().grouped(2).map(l => (l(0), l(1))).toList.unzip
    g.fillPolygon(xPoints, yPoints)

    val childRadius = 20
    chars.zip(colors).zip(offsets).foreach {
      case ((letter, color), offset) =>
        drawCharCircle(letter, color, x + r + offset._1 - childRadius,
          y + r + offset._2 - childRadius, childRadius, g)
    }
  }

  def drawCharCircle(c: String, circleColor: Color, x: Int, y: Int,
    r: Int, g: G) {
    g.setColor(circleColor)
    g.fillOval(x, y, r*2, r*2)

    g.drawString(font, x + r - g.getWidth(font, c) /2 - 1,
      y + r - g.getHeight(font, c)/2 - 1, c, Color.white)
  }
}
