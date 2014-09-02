//****************************************************************************
// Hand class.  
//****************************************************************************
// Comments : 
//   Support Routines for a Simple Hand Model. To complete the assignment,
//   add in the necessary data structure and code to implement functionalities
//   for manipulating and drawing the hand model. As a place holder, we have
//   draw a simple ellipse for demonstration purposes.
//     
//
// History  :
//   16 Jan 08 Created by Tai-Peng Tian (tiantp@gmail.com) based on C code by
//   Stan Sclaroff (from CS480 '06 Hand.c )
//
//

import com.sun.opengl.util.GLUT;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.util.*;

public class Hand 
{

  public enum JointType { DISTAL, PALM, MIDDLE };
  public enum AxisType { X, Y, Z };
  public enum FingerType { FIRST, SECOND, THIRD, FOURTH, FIFTH };

  private static final double DELTA_ANGLE = 1.0;
  
  private GLUT glut ;
  private boolean state_has_changed;

  // State variables for the Hand Object. Feel free to augment with more data
  // structure of your choice.
  float scale; // control the scale of the Ellipse
  private int hand_object;
  private AxisType active_rotation_axis;
  private JointType active_joint;
  private boolean active_fingers[]; 

  public Hand( float _scale )
  {
   
    scale = _scale;

    // Initialization for the data structure
    active_fingers = new boolean [5];
    for (int i=0; i<5; ++i) active_fingers[i] =false;

    active_rotation_axis = AxisType.X;
    active_joint = JointType.DISTAL;
    state_has_changed = true;

  }

  public void init( GL gl)
  {
    hand_object = gl.glGenLists(1);
    update( gl );
  }

  // This function will construct the call list for drawing the hand
  public void update( GL gl)
  {
    if ( state_has_changed )
    {
      gl.glNewList( hand_object, GL.GL_COMPILE );
      
      // You need to rewrite the code for constructing the display
      // list 

      // Create an ellipsoid by scaling a sphere
      gl.glColor3f( 0.8f, 0.5f, 0.2f );
      GLUT glut = new GLUT();
      gl.glScalef( 1.0f, 1.0f, 0.5f );
      glut.glutSolidSphere( scale, 36, 18 );

      gl.glEndList();

      state_has_changed = false;
    }
  }

  // Toggle which finger(s) are affected by the current rotation
  public void toggle_finger( FingerType finger)
  {
    int loc=0;

    switch ( finger )
    {
      case FIRST : loc = 0; 
                   System.out.println(" Toggling First Finger");
                   break;
      case SECOND: loc = 1; 
                   System.out.println(" Toggling Second Finger");
                   break;
      case THIRD : loc = 2; 
                   System.out.println(" Toggling Third Finger");
                   break;
      case FOURTH: loc = 3; 
                   System.out.println(" Toggling Fourth Finger");
                   break;
      case FIFTH : loc = 4; 
                   System.out.println(" Toggling Fifth Finger");
                   break;
    }
    active_fingers[loc] = !active_fingers[loc];
  }

  // Setting the active joint and rotation axis
  public void set_joint( JointType joint ) 
  { 
    switch ( joint )
    {
      case DISTAL : System.out.println( " Distal Chosen ");
                    break;
      case PALM   : System.out.println( " Palm Chosen ");
                    break;
      case MIDDLE : System.out.println( " Middle Chosen ");
                    break;
    }
    active_joint = joint; 
  }

  public void set_rotation_axis( AxisType axis) 
  { 
    switch ( axis )
    {
      case X : System.out.println( "X-axis chosen");
               break;
      case Y : System.out.println( "Y-axis chosen");
               break;
      case Z : System.out.println( "Z-axis chosen");
               break;
    }
    active_rotation_axis = axis; 
  }

  // increment the rotation angle
  public void increment_rotation_angle()
  {
    state_has_changed = true; // flag to recreate the display list

    // you will need to rewrite this function
    System.out.println("Hand::increment_rotation_angle()");
  }

  // decrement the rotation angle
  public void decrement_rotation_angle()
  {
    state_has_changed = true; // flag to recreate the display list

    // you will need to rewrite this function
    System.out.println("Hand::decrement_rotation_angle()");
  }
  

  public void draw(GL gl)
  {   
    // draw the hand based on the current state of the hand
    gl.glCallList(hand_object);
  }
}

