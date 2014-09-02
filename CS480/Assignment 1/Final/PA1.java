// ****************************************************************************
// Example Main Program for CS480 Programming Assignment 1
// ****************************************************************************
// Description:
//   
// This is a template program for the polygon tool. It supports drawing a
// polygon vertices, moving vertices, reporting whether the polygon is
// concave or convex, and testing points are inside/outside the current
// polygon.
//
// LEFTMOUSE: add polygon vertex
// RIGHTMOUSE: move closest vertex
// MIDDLEMOUSE: click to see if point is inside or outside polygon
//
// The following keys control the program:
//
// Q,q: quit
// T,t: cycle through test cases
// F,f: toggle polygon fill off/on
// C,c: clear polygon (set vertex count=0)
// A,a: toggle applying inside outside test on all pixels
//
// ****************************************************************************
// History :
// 7 February 2011 - updated code to be more Java-y
// 9 Jan 2008 Created by Tai-Peng Tian (tiantp@gmail.com) based on the C
// code by Stan Sclaroff
//
package P1;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.glu.GLU;
import javax.swing.JFrame;

import com.sun.opengl.util.FPSAnimator;
import com.sun.opengl.util.GLUT;

/**
 * The driver program which runs the polygon sketching tool.
 */
public class PA1 extends JFrame implements GLEventListener, KeyListener,
    MouseListener, MouseMotionListener {
  /** The default width of the lines used to draw polygons. */
  public static final float DEFAULT_LINE_WIDTH = 1.0f;
  /** The default height of the window. */
  public static final int DEFAULT_WINDOW_HEIGHT = 512;
  /** The default width of the window. */
  public static final int DEFAULT_WINDOW_WIDTH = 512;
  /** Randomly generated serial version UID. */
  private static final long serialVersionUID = 2585038734149359171L;

  /**
   * Initializes the window and OpenGL components, then runs the animator.
   * 
   * @param args
   *          This parameter is ignored.
   */
  public static void main(String[] args) {
    final PA1 P = new PA1();
    P.run();
  }

  /** The animator used for animating the canvas. */
  FPSAnimator animator;
  /** The canvas used for drawing. */
  private GLCanvas canvas;
  /** The capabilities of this canvas. */
  private GLCapabilities capabilities = new GLCapabilities();
  /** Whether the mouse is currently dragging a selected vertex. */
  private boolean dragging = false;
  /** Whether to fill the polygon using OpenGL. */
  private boolean fill = false;
  /** A GLU object. */
  private GLU glu = new GLU();
  /** A GLUT object. */
  private GLUT glut = new GLUT();
  /**
   * The message which displays whether a selected point is inside or outside
   * the polygon.
   */
  private String insideOutsideMessage = null;
  /** Whether to highlight pixels which are inside the polygon. */
  private boolean insideOutsideTest = false;
  /** The polygon to draw on the canvas. */

  private Polygon[] polygons = new Polygon [2]; 
  private int selectedPolyIndex = 0;
 
  /** The polygon test cases. */
  private final TestCases testCases = new TestCases();

  /**
   * Initializes the OpenGL objects for GL capabilities, canvas, animators, etc.
   */
  private PA1() {
    // enables double buffering
    this.capabilities.setDoubleBuffered(true);
    this.capabilities.setStencilBits(8);
    
    this.canvas = new GLCanvas(this.capabilities);
    this.canvas.addGLEventListener(this);
    this.canvas.addMouseListener(this);
    this.canvas.addMouseMotionListener(this);
    this.canvas.addKeyListener(this);
    // auto swap buffer mode is true by default, but we are explicit here
    this.canvas.setAutoSwapBufferMode(true);
    this.getContentPane().add(this.canvas);

    // drive the display loop at 60 frames per second
    this.animator = new FPSAnimator(this.canvas, 60);

    this.setTitle("CS480/CS680 Skeleton Polygon Tool");
    this.setSize(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setVisible(true);
    
    // set up initial state of two polygons
    this.polygons[0] = new Polygon((float)1.0,(float)0.7,(float)0.5);
    this.polygons[1] = new Polygon((float)0.5,(float)1.0,(float)0.2);
    this.selectedPolyIndex = 0;
    
  }

  /**
   * Draws the polygon, fills the polygon if necessary, and displays the
   * appropriate messages on the canvas.
   * 
   * @param drawable
   *          {@inheritDoc}
   */
  public void display(final GLAutoDrawable drawable) {
    final GL gl = drawable.getGL();
    Color color;
    
    // check if we need to fill the polygon
    if (this.fill)
      gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
    else
      gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_LINE);

    // clear the display
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_STENCIL_BUFFER_BIT);

    // check if we need to highlight pixels which are inside the polygon
    if (this.insideOutsideTest) {
      // push the current color
      gl.glPushAttrib(GL.GL_CURRENT_BIT);
      gl.glPointSize(3);
      gl.glBegin(GL.GL_POINTS);
      for (int y = 0; y < this.canvas.getHeight(); y++) {
        for (int x = 0; x < this.canvas.getWidth(); x++) {
          if (this.polygons[selectedPolyIndex].isInside(x, y))
            // the inside is red
            gl.glColor3f(1.0f, 0.2f, 0.2f);
          else
            // the outside is gray
            gl.glColor3f(0.1f, 0.1f, 0.1f);
          gl.glVertex2f(x, y);
        }
      }
      gl.glEnd();
      // pop the current color
      gl.glPopAttrib();
    }

    // render the polygon_1 and polygon_2

    PolygonDrawer.draw(drawable, this.polygons[0]); 
    PolygonDrawer.draw(drawable, this.polygons[1]);


    // display a message for concavity/convexity for each polygon
    gl.glPushAttrib(GL.GL_CURRENT_BIT); // push attributes to set scope of color changes
    if(this.polygons[0].vertices().size()>2){
    	color = this.polygons[0].getColor();
    	gl.glColor3f(color.getRed(),color.getGreen(),color.getBlue());
    	if (this.polygons[0].isConcave()) 
    		drawString(drawable, 20, this.canvas.getHeight() - 80,
    				"Polygon 1 is concave", GLUT.BITMAP_HELVETICA_18);
    	else
    		drawString(drawable, 20, this.canvas.getHeight() - 80,
    				"Polygon 1 is convex", GLUT.BITMAP_HELVETICA_18);
    	if (this.polygons[0].clockWise)
    		drawString(drawable, 195, this.canvas.getHeight() - 80,
    				"and clockwise", GLUT.BITMAP_HELVETICA_18);
    	if (this.polygons[0].counterClockwise)
    		drawString(drawable, 195, this.canvas.getHeight() - 80,
    				"and counter clockwise", GLUT.BITMAP_HELVETICA_18);
    }
   
    if(this.polygons[1].vertices().size()>2){
    	color = this.polygons[1].getColor();
    	gl.glColor3f(color.getRed(),color.getGreen(),color.getBlue());
    	if (this.polygons[1].isConcave()) 
    		drawString(drawable, 20, this.canvas.getHeight() - 60,
    				"Polygon 2 is concave", GLUT.BITMAP_HELVETICA_18);
    	else
    		drawString(drawable, 20, this.canvas.getHeight() - 60,
    				"Polygon 2 is convex", GLUT.BITMAP_HELVETICA_18);
    	
    	if (this.polygons[1].clockWise)
    		drawString(drawable, 195, this.canvas.getHeight() - 60,
    				"and clockwise", GLUT.BITMAP_HELVETICA_18);
    	if (this.polygons[1].counterClockwise)
    		drawString(drawable, 195, this.canvas.getHeight() - 60,
    				"and counter clockwise", GLUT.BITMAP_HELVETICA_18);
    
    gl.glPopAttrib(); // pop attributes to restore
    
    if(this.polygons[0].isOverlapping(this.polygons[1]))
    	drawString(drawable, 20, this.canvas.getHeight() - 40,
    			"Polygons overlap", GLUT.BITMAP_HELVETICA_18);
    else	
    	drawString(drawable, 20, this.canvas.getHeight() - 40,
    			"Polygons do not overlap", GLUT.BITMAP_HELVETICA_18);
      
    if (this.insideOutsideMessage != null)
      drawString(drawable, 20, this.canvas.getHeight() - 20,
          this.insideOutsideMessage, GLUT.BITMAP_HELVETICA_18);
    }
 
    
  }

  /**
   * {@inheritDoc}
   * 
   * @param drawable
   *          {@inheritDoc}
   * @param modeChanged
   *          {@inheritDoc}
   * @param deviceChanged
   *          {@inheritDoc}
   */
  public void displayChanged(GLAutoDrawable drawable, boolean modeChanged,
      boolean deviceChanged) {
    // intentionally unimplemented
  }

  /**
   * Draws a String to the canvas.
   * 
   * @param drawable
   *          The drawable on which to draw the String.
   * @param x
   *          The x location to draw the String.
   * @param y
   *          The y location to draw the String.
   * @param s
   *          The String to draw.
   * @param font
   *          The font with which to draw the String, as chosen from GLUT
   *          constants.
   */
  private void drawString(final GLAutoDrawable drawable, final int x,
      final int y, final String s, final int font) {
    drawable.getGL().glRasterPos2f(x, y);
    this.glut.glutBitmapString(font, s);
  }

  /**
   * {@inheritDoc}
   * 
   * @param drawable
   *          {@inheritDoc}
   */
  public void init(final GLAutoDrawable drawable) {
    final GL gl = drawable.getGL();
    gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl.glLineWidth(DEFAULT_LINE_WIDTH);
  }

  /**
   * {@inheritDoc}
   * 
   * @param key
   *          {@inheritDoc}
   */
  public void keyPressed(KeyEvent key) {
    switch (key.getKeyCode()) {
    case KeyEvent.VK_ESCAPE:
      new Thread() {
        @Override
        public void run() {
          PA1.this.animator.stop();
        }
      }.start();
      System.exit(0);
      break;
    default:
      break;
    }
  }

  /**
   * This method is intentionally unimplemented.
   * 
   * @param key
   *          This parameter is ignored.
   */
  public void keyReleased(final KeyEvent key) {
    // intentionally unimplemented
  }

  /**
   * {@inheritDoc}
   * 
   * The following list summarizes the actions produced on the specified keys:
   * 
   * Q or q - quit the program T or t - cycle through the test cases F or f -
   * toggle OpenGL's polygon fill C or c - clear (i.e. reset to empty) the
   * current polygon S or s - highlight pixels which are inside the polygon
   * 
   * @param key
   *          {@inheritDoc}
   */
  public void keyTyped(final KeyEvent key) {
    switch (key.getKeyChar()) {
    case 'Q':
    case 'q':
      new Thread() {
        @Override
        public void run() {
          PA1.this.animator.stop();
        }
      }.start();
      System.exit(0);
      break;

    case 'T':
    case 't':
    	this.polygons[0] = this.testCases.next();
    	this.polygons[1] = this.testCases.next();
      break;

    case 'F':
    case 'f':
      this.fill = !this.fill;
      break;

    case 'C':
    case 'c':
        // reset initial state of two polygons
    	this.polygons[0] = new Polygon((float)1.0,(float)0.7,(float)0.5);
        this.polygons[1] = new Polygon((float)0.5,(float)1.0,(float)0.2);
        this.selectedPolyIndex = 0;
        break;

    case 'S':
    case 's':
      this.insideOutsideTest = !this.insideOutsideTest;
      break;

    case 'P':
    case 'p':
      System.out.println(this.polygons[selectedPolyIndex]);
      break;
  
    case '1':
    	this.selectedPolyIndex = 0;
    	//this.selectedPolygon = this.polygon_1;
    	break;
    	
    case '2':
    	this.selectedPolyIndex = 1;
    	//this.selectedPolygon = this.polygon_2;
    	break;
    
    default:
      break;
    }
  }

  /**
   * This method is intentionally unimplemented.
   * 
   * @param mouse
   *          This parameter is ignored.
   */
  public void mouseClicked(final MouseEvent mouse) {
    // intentionally unimplemented
  }

  /**
   * If the right mouse button is being dragged, then move the selected vertex
   * to the point under the mouse event.
   * 
   * @param mouse
   *          {@inheritDoc}
   */
  public void mouseDragged(final MouseEvent mouse) {
    if (this.dragging) {
    	this.polygons[selectedPolyIndex].moveVert(mouse.getX(), mouse.getY());
    }
  }

  /**
   * This method is intentionally unimplemented.
   * 
   * @param mouse
   *          This parameter is ignored.
   */
  public void mouseEntered(MouseEvent mouse) {
    // intentionally unimplemented
  }

  /**
   * This method is intentionally unimplemented.
   * 
   * @param mouse
   *          This parameter is ignored.
   */
  public void mouseExited(MouseEvent mouse) {
    // intentionally unimplemented
  }

  /**
   * This method is intentionally unimplemented.
   * 
   * @param mouse
   *          This parameter is ignored.
   */
  public void mouseMoved(final MouseEvent mouse) {
    // intentionally unimplemented
  }

  /**
   * Respond to mouse presses.
   * 
   * On left clicks, this method adds a vertex to the current polygon at the
   * location under the current mouse press.
   * 
   * On middle clicks, this method instructs the canvas to display a message
   * stating whether the point under the mouse is inside or outside the current
   * polygon.
   * 
   * On right clicks, this method selects the vertex of the current polygon
   * nearest the mouse press to initiate dragging.
   * 
   * @param mouse
   *          {@inheritDoc}
   */
  public void mousePressed(final MouseEvent mouse) {
    int button = mouse.getButton();

    // on left mouse clicks, add a new vertex
    if (button == MouseEvent.BUTTON1)
    	this.polygons[selectedPolyIndex].addVert(mouse.getX(), mouse.getY());

    // on middle clicks, display whether the point under the mouse is inside or
    // outside the polygon
    else if (button == MouseEvent.BUTTON2) {
      if (this.polygons[selectedPolyIndex].isInside(mouse.getX(), mouse.getY()))
        this.insideOutsideMessage = "Point is INSIDE polygon";
      else
        this.insideOutsideMessage = "Point is OUTSIDE polygon";
    }

    // on right clicks, select the closest vertex
    else if (button == MouseEvent.BUTTON3) {
      int x = mouse.getX();
      int y = mouse.getY();
      this.polygons[selectedPolyIndex].selectVert(x, y);
      this.polygons[selectedPolyIndex].moveVert(x, y);
      this.dragging = true;
    }

  }

  /**
   * Remove any "inside/outside" message if the middle mouse button is released,
   * or terminate dragging a vertex if the right mouse button is released.
   * 
   * @param mouse
   *          {@inheritDoc}
   */
  public void mouseReleased(final MouseEvent mouse) {
    int button = mouse.getButton();
    if (button == MouseEvent.BUTTON2)
      this.insideOutsideMessage = null;
    else if (button == MouseEvent.BUTTON3)
      this.dragging = false;
  }

  /**
   * {@inheritDoc}
   * 
   * This method is called when the window is resized.
   * 
   * @param drawable
   *          {@inheritDoc}
   * @param x
   *          {@inheritDoc}
   * @param y
   *          {@inheritDoc}
   * @param width
   *          {@inheritDoc}
   * @param height
   *          {@inheritDoc}
   */
  public void reshape(final GLAutoDrawable drawable, final int x, final int y,
      final int width, final int height) {
    // change viewport dimensions
    final GL gl = drawable.getGL();
    gl.glViewport(0, 0, width, height);
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    this.glu.gluOrtho2D(0, width, height, 0);
    gl.glMatrixMode(GL.GL_MODELVIEW);
  }

  /** Runs the animator of the canvas. */
  public void run() {
    this.animator.start();
  }

}