package org.newdawn.slick.tests;

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.BasicGame;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.command._

import scala.sys

/**
 * A test for abstract input via InputProvider
 *
 * @author kevin
 */
class InputProviderTest extends BasicGame("InputProvider Test") with InputProviderListener {
  val attack = new BasicCommand("attack");
  val jump = new BasicCommand("jump");
  val run = new BasicCommand("run");
  val exit = new BasicCommand("exit")
  var provider: InputProvider = _;
  var input: Input = _
  var message = "";

  def init(container: GameContainer) {
    input = container.getInput()
    provider = new InputProvider(input);
    provider.addListener(this);

    provider.bindCommand(new KeyControl(Input.KEY_LEFT), run);
    provider.bindCommand(new KeyControl(Input.KEY_A), run);
    provider.bindCommand(new ControllerDirectionControl(0, ControllerDirectionControl.LEFT), run);
    provider.bindCommand(new KeyControl(Input.KEY_UP), jump);
    provider.bindCommand(new KeyControl(Input.KEY_W), jump);
    provider.bindCommand(new ControllerDirectionControl(0, ControllerDirectionControl.UP), jump);
    provider.bindCommand(new KeyControl(Input.KEY_SPACE), attack);
    provider.bindCommand(new MouseButtonControl(0), attack);
    provider.bindCommand(new ControllerButtonControl(0, 1), attack);

    provider.bindCommand(new KeyControl(Input.KEY_ESCAPE), exit)
  }

  def render(container: GameContainer, g: Graphics) {
    g.drawString("Press A, W, Left, Up, space, mouse button 1,and gamepad controls",10,50);
    g.drawString(message,100,150);
  }

  def update(container: GameContainer, delta: Int) {
  }

  def controlPressed(command: Command) {
    message = "Pressed: "+command;
  }

  def controlReleased(command: Command) {
    command match {
      case `exit` => sys.exit(0)
      case _ => message = "Released: "+command;
    }
  }

  override def controllerButtonPressed(controller: Int, button: Int) {
    super.controllerButtonPressed(controller, button)

    println(s"button $button pressend on controller $controller")
    input.poll(800, 600)

    val axisValues = (0 until input.getAxisCount(controller)).map(axis =>
      input.getAxisValue(controller, axis))
    println("axises: " + axisValues.map(value => "%.3f".format(value)).mkString(" "))
  }
}

import java.io.File
import org.lwjgl.LWJGLUtil

object Game extends App {
  System.setProperty("org.lwjgl.librarypath", new File(new File(new File(System.getProperty("user.dir"), "lib"), "native"), LWJGLUtil.getPlatformName()).getAbsolutePath());
  System.setProperty("net.java.games.input.librarypath", System.getProperty("org.lwjgl.librarypath"));

  val container = new AppGameContainer(new InputProviderTest());
  container.setDisplayMode(800,600,false);
  container.start();
}
