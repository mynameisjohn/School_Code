//****************************************************************************
//       Example Main Program for CS480 Programming Assignment 1
//****************************************************************************
// Description: 
//   
//   This is a template program for the polygon tool.  It supports drawing a
//   polygon vertices, moving vertices, reporting whether the polygon is
//   concave or convex, and testing points are inside/outside the current
//   polygon.
//
//     LEFTMOUSE: add polygon vertex 
//     RIGHTMOUSE: move closest vertex 
//     MIDDLEMOUSE: click to see if point is inside or outside poly 
//
//     The following keys control the program:
//
//      Q,q: quit
//      T,t: cycle through test cases
//      F,f: toggle polygon fill off/on
//      C,c: clear polygon (set vertex count=0)
//      A,a: toggle applying inside outside test on all pixels
//
//****************************************************************************
// History :
//   9 Jan 2008 Created by Tai-Peng Tian (tiantp@gmail.com) based on the C
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


public class PA1 extends JFrame
  implements GLEventListener, KeyListener, MouseListener, MouseMotionListener
{
  private final int DEFAULT_WINDOW_WIDTH=512;
  private final int DEFAULT_WINDOW_HEIGHT=512;
  private final float DEFAULT_LINE_WIDTH=1.0f;

  private GLCapabilities capabilities;
  private GLCanvas canvas;
  private FPSAnimator animator;
  private GLU glu;
  private GLUT glut;
  private Polygon poly;
  private boolean fill; // fill up polygon when drawing
  private String msg1, msg2; // message to be displayed at top left corner
  private boolean disp_msg2;
  private boolean move_vert; // control mouse behavior
  private boolean show_io_all; // apply inside outside test on all pixels

  private int numTestCase;
  private int testCase;


  public PA1()
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

    poly = new Polygon();
    glu  = new GLU();
    glut = new GLUT();
    fill = false;

    msg1 = "";
    msg2 = "";
    disp_msg2 = false;
    move_vert = false;
    numTestCase = 11;
    testCase = 0;
    show_io_all = false;

    setTitle("CS480/CS680 Skeleton Polygon Tool");
    setSize( DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setVisible(true);
  }

  public void run()
  {
    animator.start();
  }

  public static void main( String[] args )
  {
    PA1 P = new PA1();
    P.run();
  }

  //*********************************************** 
  //  GLEventListener Interfaces
  //*********************************************** 
  public void init( GLAutoDrawable drawable) 
  {
    GL gl = drawable.getGL();
    gl.glClearColor( 0.0f, 0.0f, 0.0f, 0.0f);
    gl.glLineWidth( DEFAULT_LINE_WIDTH );
  }

  // Redisplaying graphics
  public void display(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();

    /* If requested, enable fill */
    if (fill == true )
      gl.glPolygonMode( GL.GL_FRONT_AND_BACK, GL.GL_FILL );
    else
      gl.glPolygonMode( GL.GL_FRONT_AND_BACK, GL.GL_LINE );

    /* clear the display */
    gl.glClear( GL.GL_COLOR_BUFFER_BIT );

    /* display the inside outside test for each pixel */
    if (show_io_all == true )
    {
      int height = canvas.getHeight();
      int width  = canvas.getWidth();
    
      gl.glPushAttrib(GL.GL_CURRENT_BIT); /* push the current color */
      gl.glPointSize(3);
      gl.glBegin( GL.GL_POINTS );
      for ( int y=0; y<height; y++)
      {
        for (int x=0; x<width; x++)
        {
          if ( poly.insidePoly( x, y ) == true )
            gl.glColor3f( 1.0f, 0.2f, 0.2f ); /* inside is red */
          else
            gl.glColor3f( 0.1f, 0.1f, 0.1f ); /* outside is gray */
          gl.glVertex2f( x,y);
        }
      }
      gl.glEnd();
      gl.glPopAttrib(); /* pop current color */
    }

    /* render the polygon */
    if ( poly.concavePoly() == false )
    {
        poly.drawConvexPoly( drawable );
        msg1 = "Polygon is convex"; 
    }
    else
    {
        poly.drawConcavePoly( drawable );
        msg1 = "Polygon is concave";
    }

    drawString( drawable, 20, canvas.getHeight() - 60, 
        msg1, GLUT.BITMAP_TIMES_ROMAN_24);
    if (disp_msg2 == true )
        drawString( drawable, 20, canvas.getHeight() - 20, 
            msg2, GLUT.BITMAP_TIMES_ROMAN_24); 
  }

  // Window size change
  public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h)
  {
    // Change viewport dimensions
    GL gl = drawable.getGL();
    gl.glViewport(0, 0, w, h);
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    glu.gluOrtho2D(0, w, h, 0);
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
  //      Q,q: quit
  //      T,t: cycle through test cases
  //      F,f: toggle polygon fill off/on
  //      C,c: clear polygon (set vertex count=0)
  //      S,s: toggle applying inside outside test on each pixel

      switch ( key.getKeyChar() ) {
        case 'Q' :
        case 'q' : new Thread() {
                     public void run()
                     { animator.stop(); }
                   }.start();
                   System.exit(0);
                   break;
        case 'T' :
        case 't' : generateTestCase();
                   break;
        case 'F' :
        case 'f' : fill = !fill; 
                   break;
        case 'C' :
        case 'c' : poly.reset(); 
                   break;
        case 'S' :
        case 's' : show_io_all = !show_io_all;
                   break;

        case 'P' :
        case 'p' : poly.print();
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

    // Add a new vertex using left mouse click
    if ( button  == MouseEvent.BUTTON1 )
      poly.addVert( mouse.getX(), mouse.getY() );

    // Conduct Inside / Outside test using center mouse click
    else if ( button == MouseEvent.BUTTON2 )
    {
      disp_msg2 = true;
      if (poly.insidePoly( mouse.getX(), mouse.getY()) == true )
        msg2 = "Point is INSIDE polygon";
      else
        msg2 = "Point is OUTSIDE polygon";
    }

    // Pick the vertex closest to mouse down event using right button
    else if ( button == MouseEvent.BUTTON3 )
    {
      int x = mouse.getX();
      int y = mouse.getY();
      poly.selectVert( x, y );
      poly.moveVert( x, y );
      move_vert = true;
    }

  }

  public void mouseReleased(MouseEvent mouse)
  {
    int button = mouse.getButton();
    if ( button == MouseEvent.BUTTON2 ) 
      disp_msg2 = false;

    else if (button == MouseEvent.BUTTON3)
      move_vert = false;
  }

  public void mouseMoved( MouseEvent mouse)
  {
  }

  public void mouseDragged( MouseEvent mouse )
  {
  
    int button = mouse.getButton();

    // Move the vertex around
    if ( move_vert == true  )
    {
      int x = mouse.getX();
      int y = mouse.getY();
      poly.moveVert( x, y );
    }
  }

  public void mouseEntered( MouseEvent mouse)
  {
  }

  public void mouseExited( MouseEvent mouse)
  {
  } 


  //*********************************************** 
  //  Helper functions 
  //*********************************************** 
  public void drawString( GLAutoDrawable drawable, 
      int x, int y, String s, int font)
  {
    GL gl = drawable.getGL();

    gl.glRasterPos2f( x, y);
    glut.glutBitmapString( font, s );
  }

  public void generateTestCase()
  {
    switch (testCase)
    {
        case 0: poly.reset();
                poly.addVert( 50, 50);
                poly.addVert( 50,100);
                poly.addVert(100,100);
                poly.addVert(100, 50);
                break;
        
        case 1: poly.reset();
                poly.addVert( 50, 50);
                poly.addVert(250, 50);
                poly.addVert(250,200);
                poly.addVert(150,125);
                poly.addVert( 50,200);
                break;

        case 2: poly.reset(); 
                poly.addVert( 50, 200);
                poly.addVert( 50, 50);
                poly.addVert(200, 50);
                poly.addVert(200, 100);
                poly.addVert(100, 100);
                poly.addVert(100, 200);
                break;

        case 3: poly.reset();
                poly.addVert( 50,  50);
                poly.addVert(200,  50);
                poly.addVert(200, 200);
                poly.addVert(150, 200);
                poly.addVert(150, 100);
                poly.addVert( 50, 100);
                break;
                
        case 4: poly.reset();
                poly.addVert( 50, 100);
                poly.addVert(100, 100);
                poly.addVert(100,  50);
                poly.addVert(150,  50);
                poly.addVert(150, 100);
                poly.addVert(200, 100);
                poly.addVert(200, 150);
                poly.addVert(150, 150);
                poly.addVert(150, 200);
                poly.addVert(100, 200);
                poly.addVert(100, 150);
                poly.addVert( 50, 150);
                break;

        case 5: poly.reset(); 
                poly.addVert(50,250);
                poly.addVert(50,150);
                poly.addVert(100,150);
                poly.addVert(150,50);
                poly.addVert(200,200);
                poly.addVert(250,50);
                poly.addVert(275,150);
                poly.addVert(275,250);
                break;

        case 6: poly.reset();
                poly.addVert(50,50);
                poly.addVert(50,200);
                poly.addVert(75,200);
                poly.addVert(75,100);
                poly.addVert(100,100);
                poly.addVert(100,200);
                poly.addVert(125,200);
                poly.addVert(125,100);
                poly.addVert(150,100);
                poly.addVert(150,200);
                poly.addVert(175,200);
                poly.addVert(175,100);
                poly.addVert(275,100);
                poly.addVert(275,5);
                poly.addVert(250,5);
                poly.addVert(250,50);
                poly.addVert(225,50);
                poly.addVert(225,5);
                poly.addVert(200,5);
                poly.addVert(200,50);
                poly.addVert(175,50);
                poly.addVert(175,5);
                poly.addVert(150,5);
                poly.addVert(150,50);
                break;

        case 7: poly.reset(); 
                poly.addVert(37, 223);
                poly.addVert(37, 143);
                poly.addVert(91, 143);
                poly.addVert(113, 84);
                poly.addVert(156,192);
                poly.addVert(191, 84);
                poly.addVert(244,143);
                poly.addVert(244, 44);
                poly.addVert(17,  44);
                poly.addVert(17,  10);
                poly.addVert(268, 10);
                poly.addVert(268,223);
                break;

        case 8: poly.reset();
                poly.addVert(50,100);
                poly.addVert(250,100);
                poly.addVert(200,50);
                poly.addVert(150,150);
                poly.addVert(100,50);
                break;

        case 9: poly.reset();
                poly.addVert(131, 54);
                poly.addVert(52, 207);
                poly.addVert(134, 146);
                poly.addVert(226, 207);
                break;

        case 10:  poly.reset(); 
                poly.addVert(74, 112);
                poly.addVert(212, 141);
                poly.addVert(96, 150);
                poly.addVert(142, 221);
                poly.addVert(135, 60);
                break;

        case 11: poly.reset(); 
                poly.addVert(9, 260);
                poly.addVert(96, 114);
                poly.addVert(168, 178);
                poly.addVert(242, 99);
                poly.addVert(16, 99);
                poly.addVert(13, 20);
                poly.addVert(279, 24);
                poly.addVert(279, 261);
                break;

        default: 
          break;

    }

    if (testCase > numTestCase)
      testCase = 0;
    else 
      ++testCase;
 }
}
