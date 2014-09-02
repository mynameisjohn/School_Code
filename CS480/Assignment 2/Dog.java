/**
 * Dog.java - dog model which can draw itself in OpenGL
 * 
 * History:
 * 
 * 18 February 2011
 * 
 * - added documentation
 * 
 * (Jeffrey Finkelstein <jeffrey.finkelstein@gmail.com>)
 * 
 * 16 January 2008
 * 
 * modified to dog class by Stan Sclaroff in February, 2012
 * 
 * (Stan Sclaroff <sclaroff@cs.bu.edu>)
 */


import javax.media.opengl.GL;

import com.sun.opengl.util.GLUT;

/**
 * A model of a dog containing an ellipsoid for the body, four legs, a neck, head, and tail.
 * 
 * This class provides methods for rotating individual joints on individual
 * legs as well.
 * 
 * @author Stan Sclaroff
 * @since Spring 2012
 */
public class Dog {

  /** The x-, y-, or z-axis. */
  enum AxisType {
    X, Y, Z
  }

  /** The body part to toggle. */
  enum BodyPartType {
    FRONT_LEFT_LEG, FRONT_RIGHT_LEG, REAR_LEFT_LEG, REAR_RIGHT_LEG, NECK, HEAD, TAIL
  }

  /** The type of joint at which to enable rotation. */
  enum JointType {
    PAW, UPPER, LOWER
  }

  /**
   * The change in rotation angle (in degrees) to apply on each rotation update.
   */
  private static final double DELTA_ANGLE = 1.0;

  /** The currently active parts. */
  private final boolean active_parts[] = new boolean[] { false, false,
      false, false, false, false, false };
  /** The currently active joint type. */
  private JointType active_joint = JointType.PAW;
  /** The currently active axis of rotation. */
  private AxisType active_rotation_axis = AxisType.X;
  /** The OpenGL handle (integer) for the OpenGL call list object. */
  private int dog_object;
  /**
   * Whether the state of the dog model has changed since the last update.
   * 
   * This is initially true so that the dog will show up initially.
   */
  private boolean state_has_changed = true;

  private final GLUT glut;

  /**
   * Instantiates this dog with access to the specified OpenGL utility toolkit
   * object.
   * 
   * @param glut
   *          The OpenGL utility toolkit object.
   */
  public Dog(final GLUT glut) {
    this.glut = glut;
  }

  /**
   * Decreases the rotation of the currently selected joint on the currently
   * selected body parts by {@value #DELTA_ANGLE} degrees.
   */
  public void rotateActiveJointsForward() {
    this.state_has_changed = true; // flag to recreate the display list

    // you will need to rewrite this function
    System.out.println("Dog::decrement_rotation_angle()");
  }

  /**
   * Draws the god on the specified OpenGL object based on the current state.
   * 
   * @param gl
   *          The GL object with which to draw the dog.
   */
  public void draw(final GL gl) {
    gl.glCallList(this.dog_object);
  }

  /**
   * Increases the rotation of the currently selected joint on the currently
   * selected body parts by {@value #DELTA_ANGLE} degrees.
   */
  public void rotateActiveJointsBackward() {
    this.state_has_changed = true; // flag to recreate the display list

    // you will need to rewrite this function
    System.out.println("Dog::increment_rotation_angle()");
  }

  /**
   * Uses the specified OpenGL object to create a new OpenGL call list on which
   * to draw the dog's body, legs, tail, head, and neck.
   * 
   * @param gl
   *          The OpenGL object from which to get a handle (which is just a
   *          unique integer) for a call list.
   */
  public void init(final GL gl) {
    this.dog_object = gl.glGenLists(1);
    this.update(gl);
  }

  /**
   * Activates the specified type of joint for rotation.
   * 
   * @param joint
   *          The type of joint to rotate.
   */
  public void set_joint(final JointType joint) {
    switch (joint) {
    case PAW:
      System.out.println(" Paw Chosen ");
      break;
    case UPPER:
      System.out.println(" Upper Chosen ");
      break;
    case LOWER:
      System.out.println(" Lower Chosen ");
      break;
    }
    this.active_joint = joint;
  }

  /**
   * Sets the axis around which to rotate.
   * 
   * @param axis
   *          The axis around which to rotate.
   */
  public void set_rotation_axis(AxisType axis) {
    switch (axis) {
    case X:
      System.out.println("X-axis chosen");
      break;
    case Y:
      System.out.println("Y-axis chosen");
      break;
    case Z:
      System.out.println("Z-axis chosen");
      break;
    }
    this.active_rotation_axis = axis;
  }

  /**
   * Toggles the specified body part to be affected by rotations.
   * 
   * @param bodyPart
   *          The bodyPart to toggle.
   */
  public void toggle_part(BodyPartType bodyPart) {
    int loc = 0;

    switch (bodyPart) {
    case REAR_LEFT_LEG:
      loc = 0;
      System.out.println(" Toggling rear left leg");
      break;
    case REAR_RIGHT_LEG:
      loc = 1;
      System.out.println(" Toggling rear right leg");
      break;
    case FRONT_LEFT_LEG:
      loc = 2;
      System.out.println(" Toggling front left leg");
      break;
    case FRONT_RIGHT_LEG:
      loc = 3;
      System.out.println(" Toggling front right leg");
      break;
    case HEAD:
      loc = 4;
      System.out.println(" Toggling head");
      break;
    case TAIL:
        loc = 5;
        System.out.println(" Toggling tail");
        break;
    case NECK:
        loc = 6;
        System.out.println(" Toggling neck");
        break;
    }
    this.active_parts[loc] = !this.active_parts[loc];
  }

  /**
   * Updates the current model of the dog.
   * 
   * @param gl
   *          The OpenGL object with which to draw the dog.
   */
  public void update(final GL gl) {
    if (this.state_has_changed) {
      gl.glNewList(this.dog_object, GL.GL_COMPILE);

      /**
       * You will need to rewrite the following code in order to display the
       * dog with its legs, tail, neck and head.
       */

      // create an ellipsoid by scaling a sphere
      gl.glColor3f(0.82f, 0.71f, 0.67f);
      gl.glScalef(1.0f, 0.5f, 0.5f);
      this.glut.glutSolidSphere(1, 36, 18);

      gl.glEndList();

      // reset the state_has_changed flag
      this.state_has_changed = false;
    }
  }
}
