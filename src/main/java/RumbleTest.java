  /*
   * RumbleTest.java
   *
   * Created on 01 December 2003, 23:02
   */
  package org.furidamu.SpaceMarauders;

  import net.java.games.input.ControllerEnvironment;
  import net.java.games.input.Controller;
  import net.java.games.input.Rumbler;
  import net.java.games.input.Version;

  /**
   *
   * @author  Jeremy
   */
  public class RumbleTest {

      public static void rumble(int c, float strength) {
        ControllerEnvironment ca = ControllerEnvironment.getDefaultEnvironment();
        Controller[] controllers = ca.getControllers();
        Rumbler[] rumblers = controllers[c].getRumblers();
        for(int j=0;j<rumblers.length;j++) {
          rumblers[j].rumble(strength);
        }
      }

      /** Creates a new instance of RumbleTest */
      public RumbleTest() {
          ControllerEnvironment ca = ControllerEnvironment.getDefaultEnvironment();
          System.out.println("JInput version: " + Version.getVersion());
          Controller[] controllers = ca.getControllers();
          for(int i=0;i<controllers.length;i++) {
              System.out.println("Scanning " + controllers[i].getName() + ": " + i);
              System.out.println("Scanning " + controllers[i].getType());
              Rumbler[] rumblers = controllers[i].getRumblers();
              System.out.println("Found " + rumblers.length + " rumblers");
              for(int j=0;j<rumblers.length;j++) {
                  System.out.println("Rumbler " + rumblers[j].getAxisName() + " on axis " + rumblers[j].getAxisIdentifier());
                  System.out.println("Rumbling with intensity: " + 0.5f);
                  rumblers[j].rumble(0.5f);
                  try {
                      Thread.sleep(1000);
                  } catch (InterruptedException e) {
                  }
                  System.out.println("Rumbling with intensity: " + 1.0f);
                  rumblers[j].rumble(1f);
                  try {
                      Thread.sleep(1000);
                  } catch (InterruptedException e) {
                  }
                  System.out.println("Fading rumble to -1");
                  for(float k=1.0f;k>-1.0f;) {
                      long startTime = System.currentTimeMillis();
                      rumblers[j].rumble(k);
                      try {
                          Thread.sleep(1);
                      } catch (InterruptedException e) {
                      }
                      k-=((float)(System.currentTimeMillis() - startTime))/1000f;
                  }
                  try {
                      Thread.sleep(1000);
                  } catch (InterruptedException e) {
                  }
                  System.out.println("Rumbling with intensity: " + 0.0f);
                  rumblers[j].rumble(0f);
                  try {
                      Thread.sleep(1000);
                  } catch (InterruptedException e) {
                  }
              }
          }
          System.exit(0);
      }
  }
