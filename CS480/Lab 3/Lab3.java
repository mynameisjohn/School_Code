//****************************************************************************
//       Example Main Program for CS480 Programming Assignment 2
//****************************************************************************
// Description: This is a simple example program that allows the  
//              user to display a hand model and change its parameters.
// 
// 	     The following keys control the program:
// 
//              Q,q, <escape>: quit
//              R: reset viewing angle
// 
//              Left mouse click + drag motion: rotate the hand view
// 
//              1 : toggle the first finger (thumb) active in rotation
//              2 : toggle the second finger active in rotation
//              3 : toggle the third finger active in rotation
//              4 : toggle the fourth finger active in rotation
//              5 : toggle the fifth finger active in rotation
// 
//              X : use the X axis rotation at the active joint(s)
//              Y : use the Y axis rotation at the active joint(s)
//              Z : use the Z axis rotation at the active joint(s)
// 
//              P : select joint that connects finger to palm 
//              M : select middle joint 
//              D : select last (distal) joint
// 
//              up-arrow, down-arrow: increase/decrease rotation angle
// 
//****************************************************************************
// History :
//   16 Jan 2008 Created by Tai-Peng Tian (tiantp@gmail.com) based on the C
//   code by Stan Sclaroff
//

import javax.swing.*;
import java.awt.*;
import java.awt.event.*; 

import com.sun.opengl.util.GLUT;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.glu.GLU;
import com.sun.opengl.util.*;


public class Lab3 extends JFrame
  implements GLEventListener, KeyListener, MouseListener, MouseMotionListener
{
  private final int DEFAULT_WINDOW_WIDTH =512;
  private final int DEFAULT_WINDOW_HEIGHT=512;

  private GLCapabilities capabilities;
  private GLCanvas canvas;
  private FPSAnimator animator;
  private GLU glu;
  private GLUT glut;
  private Hand hand;
  private Quaternion viewing_quaternion; // world rotation controlled by mouse actions

  Cube cube;

  // State variables for the mouse actions
  int last_x, last_y;
  boolean rotate_world;

  public Lab3()
  {

    capabilities = new GLCapabilities();
    capabilities.setDoubleBuffered(true);  // Enable Double buffering

    canvas  = new GLCanvas(capabilities);
    canvas.addGLEventListener(this);
    canvas.addMouseListener(this);
    canvas.addMouseMotionListener(this);
    canvas.addKeyListener(this);
    canvas.setAutoSwapBufferMode(true); // true by default. Just to be explicit
    getContentPane().add(canvas);

    animator = new FPSAnimator(canvas, 60); // drive the display loop @ 60 FPS

    glu  = new GLU();
    glut = new GLUT();

    setTitle("CS480/CS680 : Hand Simulator");
    setSize( DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setVisible(true);
   
    last_x = last_y = 0;
    rotate_world = false;

    // Set initialization code for user created classes that involves OpenGL
    // calls after here. After this line, the opengGl context will be
    // correctly initialized.
    hand = new Hand( 1.0f );
    viewing_quaternion = new Quaternion();
    cube = new Cube();
  }

  public void run()
  {
    animator.start();
  }

  public static void main( String[] args )
  {
    Lab3 P = new Lab3();
    P.run();
  }

  //***************************************************************************
  //GLEventListener Interfaces
  //***************************************************************************
  //
  // Place all OpenGL related initialization here. Including display list
  // initialization for user created classes
  //
  public void init( GLAutoDrawable drawable) 
  {
    GL gl = drawable.getGL();


    /* set up for shaded display of the hand */
    float light0_position[] = {1,1,1,0};
    float light0_ambient_color[] = {0.25f,0.25f,0.25f,1};
    float light0_diffuse_color[] = {1,1,1,1};

    gl.glPolygonMode(GL.GL_FRONT,GL.GL_FILL);
    gl.glEnable(GL.GL_COLOR_MATERIAL);
    gl.glColorMaterial(GL.GL_FRONT,GL.GL_AMBIENT_AND_DIFFUSE);

    gl.glClearColor(0.0f,0.0f,0.0f,0.0f);
    gl.glShadeModel(GL.GL_SMOOTH);
    
    /* set up the light source */
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, light0_position, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_AMBIENT, light0_ambient_color, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_DIFFUSE, light0_diffuse_color, 0);

    /* turn lighting and depth buffering on */
    gl.glEnable(GL.GL_LIGHTING);
    gl.glEnable(GL.GL_LIGHT0);
    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glEnable(GL.GL_NORMALIZE);

    hand.init( gl );
    cube.init( gl );
  }

  // Redisplaying graphics
  public void display(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();

    // clear the display 
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
    
    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();

    // rotate the world and then call world display list object 
    gl.glMultMatrixf( viewing_quaternion.to_matrix(), 0 );

    hand.update( gl );
    // hand.draw( gl );

    cube.draw( gl );
  }

  // Window size change
  public void reshape(GLAutoDrawable drawable, int x, int y, 
                            int width, int height)
  {

    // Change viewport dimensions
    GL gl = drawable.getGL();

    // Prevent a divide by zero, when window is too short (you cant make a
    // window of zero width).
    if(height == 0) height = 1;

    double ratio = 1.0f * width / height;

    // Reset the coordinate system before modifying 
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    
    // Set the viewport to be the entire window 
    gl.glViewport(0, 0, width, height);
    
    // Set the clipping volume 
    glu.gluPerspective(25,ratio,0.1,100);

    // Camera positioned at (0,0,6), look at point (0,0,0), Up Vector (0,1,0)
    glu.gluLookAt(0,0,12,0,0,0,0,1,0);

    gl.glMatrixMode(GL.GL_MODELVIEW);
    
  }

  public void displayChanged(GLAutoDrawable drawable, boolean modeChanged,
      boolean deviceChanged)
  {
  }


  //*********************************************** 
  //          KeyListener Interfaces
  //*********************************************** 
  public void keyTyped(KeyEvent key)
  {
      switch ( key.getKeyChar() ) {
        case 'Q' :
        case 'q' : new Thread() {
                     public void run()
                     { animator.stop(); }
                   }.start();
                   System.exit(0);
                   break;

        // set the viewing quaternion to 0 rotation 
        case 'R' :
        case 'r' : 
                   viewing_quaternion.reset(); 
                   break;

        // Toggle which finger(s) are affected by the current rotation
        case '1' : hand.toggle_finger( Hand.FingerType.FIRST  );
                   break;
        case '2' : hand.toggle_finger( Hand.FingerType.SECOND );
                   break;
        case '3' : hand.toggle_finger( Hand.FingerType.THIRD  );
                   break;
        case '4' : hand.toggle_finger( Hand.FingerType.FOURTH );
                   break;
        case '5' : hand.toggle_finger( Hand.FingerType.FIFTH  );
                   break;

        // select joint
        case 'D' :
        case 'd' : hand.set_joint( Hand.JointType.DISTAL);
                   break;
        case 'P' :
        case 'p' : hand.set_joint( Hand.JointType.PALM);
                   break;
        case 'M' :
        case 'm' : hand.set_joint( Hand.JointType.MIDDLE);
                   break;

         // axis of rotation at current active joint 
        case 'X' :
        case 'x' : hand.set_rotation_axis( Hand.AxisType.X);
                   break;
        case 'Z' :
        case 'z' : hand.set_rotation_axis( Hand.AxisType.Z);
                   break;

        // Demo keys
        case '6' : cube.set_choice(1);
                   break;
        case '7' : cube.set_choice(2);
                   break;
        case '8' : cube.set_choice(3);
                   break;
        case '9' : cube.set_choice(4);
                   break;

        default :
          break;
    }
 }

  public void keyPressed(KeyEvent key)
  {
    switch (key.getKeyCode()) {
      case KeyEvent.VK_ESCAPE:
        new Thread()
        {
          public void run()
          {
            animator.stop();
          }
        }.start();
        System.exit(0);
        break;

        // Up arrow key
      case KeyEvent.VK_KP_UP :
      case KeyEvent.VK_UP    :
        hand.increment_rotation_angle();
        break;

        // down arrow key
      case KeyEvent.VK_KP_DOWN :
      case KeyEvent.VK_DOWN :
        hand.decrement_rotation_angle();
        break;

      default:
        break;
    }
  }

  public void keyReleased(KeyEvent key)
  {
  }

  //************************************************** 
  // MouseListener and MouseMotionListener Interfaces
  //************************************************** 
  public void mouseClicked(MouseEvent mouse)
  {
  }

  public void mousePressed(MouseEvent mouse)
  {
    int button = mouse.getButton();
    if ( button == MouseEvent.BUTTON1 )
    {
      last_x = mouse.getX();
      last_y = mouse.getY();
      rotate_world = true;
    }
  }

  public void mouseReleased(MouseEvent mouse)
  {
    int button = mouse.getButton();
    if ( button == MouseEvent.BUTTON1 )
    {
      rotate_world = false;
    }
  }

  public void mouseMoved( MouseEvent mouse)
  {
  }

  public void mouseDragged( MouseEvent mouse )
  {
    if (rotate_world)
    {
      // vector in the direction of mouse motion
      int x = mouse.getX();
      int y = mouse.getY();
      float dx = x - last_x;
      float dy = y - last_y;
     
      // spin around axis by small delta
      float mag = (float) Math.sqrt( dx*dx + dy*dy );
      float[] axis = new float[3];
      axis[0] = dy/ mag;
      axis[1] = dx/ mag;
      axis[2] = 0.0f;

      // calculate appropriate quaternion
      float viewing_delta = 3.1415927f / 180.0f; // 1 degree
      float s = (float) Math.sin( 0.5f*viewing_delta );
      float c = (float) Math.cos( 0.5f*viewing_delta );

      Quaternion Q = new Quaternion( c, s*axis[0], s*axis[1], s*axis[2]);
      viewing_quaternion = Q.multiply( viewing_quaternion );

      // normalize to counteract acccumulating round-off error
      viewing_quaternion.normalize();

      // Save x, y as last x, y
      last_x = x;
      last_y = y;
    }
  }

  public void mouseEntered( MouseEvent mouse)
  {
  }

  public void mouseExited( MouseEvent mouse)
  {
  } 



}
