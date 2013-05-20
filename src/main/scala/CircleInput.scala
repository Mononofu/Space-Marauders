package org.furidamu.SpaceMarauders

import akka.actor._

import org.newdawn.slick._
import org.newdawn.slick.command._
import org.newdawn.slick.font.effects.{ColorEffect, Effect}
import org.newdawn.slick.geom.{Rectangle, Transform}

case class RenderCircle(hightlightedCircle: Int, leftTrigger: Boolean,
  rightTrigger: Boolean, g: Graphics)

class CircleInput {
  var circleInputActor = Config.system.actorOf(Props(new CircleInputActor), name = "circleInputActor")

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

  val lowerChars = ('a' to 'z').map(_.toString) ++ List(",", ".", ":", "/", "@", "-")
  val upperChars = lowerChars.map(_.toUpperCase)
  val numberChars = (0 to 9).map(_.toString) ++ List("?", "!", "\"", "$", "€", "%",
    "&", "*", "(", ")", "+", ";", "_", "=", "[", "]", "{", "}", "<", ">", "'", "~")
  val specialChars = (0 to 32).map(n => " ")
  val charGroupCircleRadius = 70
  val charGroupCircleOffset = 185
  val backgroundCircleRadius = charGroupCircleOffset + charGroupCircleRadius +
    charGroupCircleRadius / 2 - 5

  val font = new UnicodeFont("res/fonts/DroidSansMonoDotted.ttf", 20, true, false);
  font.addAsciiGlyphs();
  font.getEffects() match {
    // Create a default white color effect
    case effects: java.util.List[Effect] => effects.add(new ColorEffect());
  }
  font.loadGlyphs();

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


  def render(h: Int, l: Boolean, r: Boolean, g: Graphics) {
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
}
