package org.furidamu.SpaceMarauders

import org.lwjgl.input.Controllers;
import akka.actor._

case object ReadAxis
case object Poll
case object Start
case object Done

// X = Left/Right, Y = Up/Down
case class Axis(leftStickX: Float, leftStickY: Float,
  rightStickX: Float, rightStickY: Float, leftTrigger: Float, rightTrigger: Float)


object PadButton extends Enumeration {
  type PadButton = Value
  val A, B, X, Y, Back, Start, LeftBumper, RightBumper, PadLeft, PadRight, PadUp,
    PadDown, Guide, LeftStick, RightStick, Unknown = Value
}

import PadButton.PadButton

abstract class ButtonEvent extends InputEvent
case class ButtonDown(controller: Int, btn: PadButton) extends ButtonEvent
case class ButtonUp(controller: Int, btn: PadButton) extends ButtonEvent

case class Rumble(controller: Int, strength: Float)


class GamepadActor extends Actor with ActorLogging {
  Controllers.create()
  Controllers.poll()
  Controllers.clearEvents()
  val controllers = (0 until Controllers.getControllerCount()).filter(
    c => Controllers.getController(c).getAxisCount() > 10)

  val inputActor = context.actorFor("/user/inputActor")

  def receive = {
    case Poll =>
      Controllers.poll()
      while(Controllers.next()) {
        val controller = Controllers.getEventSource()
        val controllerI = controller.getIndex()
        if(controllers.contains(controllerI)) {
          val componentI = Controllers.getEventControlIndex()
          if(Controllers.isEventButton()) {
            if(controller.isButtonPressed(componentI)) {
              inputActor ! ButtonDown(controllerI, translateButton(componentI))
            } else {
              inputActor ! ButtonUp(controllerI, translateButton(componentI))
            }
          } else {
            if(componentI <= 4) {
              if(controller.getAxisValue(componentI) == 1.0) {
                inputActor ! ButtonDown(controllerI, translateAxis(componentI))
              } else {
                inputActor ! ButtonUp(controllerI, translateAxis(componentI))
              }
            } else {
              //val newValue = Controllers.getEventSource().getAxisValue(componentI)
              // new value for axis - don't create an event
            }
          }
        }
      }

    case Rumble(ctrl, strength) =>
      val controller = Controllers.getController(ctrl % 4)
      for(i <- 0 until controller.getRumblerCount()) {
        controller.setRumblerStrength(i, strength)
      }

    case ReadAxis =>
      val controllerAxes = controllers.map {
        c =>
          val controller = Controllers.getController(c)
          val axes = (0 until controller.getAxisCount()).map {
            a => controller.getAxisValue(a)
          }
          c -> Axis(axes(5), axes(6), axes(8), axes(9), axes(7), axes(10))
      }
      sender ! controllerAxes.toMap
  }

  def translateAxis(axis: Int) = axis match {
    case 0 => PadButton.Start
    case 1 => PadButton.PadLeft
    case 2 => PadButton.PadRight
    case 3 => PadButton.PadUp
    case 4 => PadButton.PadDown
  }

  def translateButton(button: Int) = button match {
    case 0 => PadButton.A
    case 1 => PadButton.B
    case 2 => PadButton.X
    case 3 => PadButton.Y
    case 4 => PadButton.LeftBumper
    case 5 => PadButton.RightBumper
    case 6 => PadButton.Back
    case 7 => PadButton.Guide
    case 8 => PadButton.LeftStick
    case 9 => PadButton.RightStick
    case _ => PadButton.Unknown
  }
}
