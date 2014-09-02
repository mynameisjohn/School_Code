/*******************************************************************
   Raytracer Previwer 
********************************************************************
History :
1 Apr 08. Created by Tai-peng Tian based on the C++ version created
          by Ashwin Thangali.
********************************************************************/

import java.io.*;
import java.util.*;

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


public class Previewer extends JFrame
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

  private RenderObjectCollection objects;
  private MaterialCollection materials;
  private LightCollection lights;
  private Camera camera;

  public Previewer ( String filename )
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

    animator = new FPSAnimator(canvas, 1); 

    glu  = new GLU();
    glut = new GLUT();

    objects   = new RenderObjectCollection();
    materials = new MaterialCollection();
    lights    = new LightCollection();
    camera    = new Camera();

    readModelFile( filename );

    setTitle("CS480/CS680 Raytracer Previewer");
    setSize( camera.viewportWidth , camera.viewportHeight );
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setVisible(true);

    // // For debugging purpose
    // camera.print();
    // objects.print();
    // lights.print();
    // materials.print();
  }

  public void run()
  {
    animator.start();
  }

  public static void main( String[] args )
  {
    if (args.length < 1) {
      System.out.println( "Usage: viewer <model_file>\n" );
      System.exit(1);
    }

    Previewer P = new Previewer( args[0] );
    P.run();
  }

  //*********************************************** 
  // Parse contents of the file 
  //*********************************************** 
  private void readModelFile( String filename)
  {
    try 
    {
      BufferedReader input = new BufferedReader( new FileReader(filename) );

      try 
      {
        String line = null;
        while ( (line = input.readLine()) != null )
        {
          StringTokenizer stok = new StringTokenizer( line );
          if (stok.hasMoreTokens())
          {
            String tok = stok.nextToken();

            if (tok.equals( "obj" )) objects.newObject( stok );
            else if (tok.equals( "light" )) lights.newLight( stok );
            else if (tok.equals( "mat" )) materials.newMaterial( stok );
            else if (tok.equals( "resolution" )) camera.setResolution( stok );
            else if (tok.equals( "camera" )) camera.setParameters( stok );
            else if (tok.equals( "viewport" )) camera.setViewport( stok );
            else if (tok.equals( "render" )) objects.setRenderObjIds( stok );
          }

        }
      }
      finally
      {
        input.close();
      }
      
    }
    catch (IOException ex) 
    {
      System.err.println( "File input error" );
    }

    objects.reorganizeObjects(); // remove objects that are not specified 
                                 // in render option.
  }

  private void draw_axes( float scale, GL gl )
  {
    

   gl.glDisable( GL.GL_LIGHTING );

    gl.glPushMatrix();
    gl.glScalef( scale, scale, scale );

    gl.glLineWidth(2);
    gl.glBegin( GL.GL_LINES );
 
    gl.glColor3f(  0.7f, 0f, 0.0f );
    gl.glVertex3f(  .8f, 0.05f, 0.0f );  
    gl.glVertex3f( 1.0f, 0.25f, 0.0f ); /* Letter X */
    gl.glVertex3f( 0.8f, .25f,  0.0f );  
    gl.glVertex3f( 1.0f, 0.05f, 0.0f );
    gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
    gl.glVertex3f( 1.0f, 0.0f, 0.0f ); /* X axis      */

    gl.glColor3f( 0.0f, 0.7f, 0.0f );
    gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
    gl.glVertex3f( 0.15f, 0.8f, 0.0f ); /* Letter Y */
    gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
    gl.glVertex3f( 0.05f, 1.0f, 0.0f );
    gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
    gl.glVertex3f( 0.25f, 1.0f, 0.0f );
    gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
    gl.glVertex3f( 0.0f, 1.0f, 0.0f ); /* Y axis      */

    gl.glColor3f( 0.0f, 0f, 0.7f );
    gl.glVertex3f( 0.05f, 0.0f , 0.8f); 
    gl.glVertex3f( 0.25f, 0.0f , 0.8f); /* Letter Z */
    gl.glVertex3f( 0.05f, 0.0f , 1.0f);  
    gl.glVertex3f( 0.25f, 0.0f , 1.0f);
    gl.glVertex3f( 0.05f, 0.0f , 0.8f);  
    gl.glVertex3f( 0.25f, 0.0f , 1.0f);
    gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
    gl.glVertex3f( 0.0f, 0.0f, 1.0f ); /* Z axis    */
    gl.glEnd();
    gl.glLineWidth(1);
    gl.glPopMatrix();

    gl.glEnable( GL.GL_LIGHTING );

  }

  //*********************************************** 
  //  GLEventListener Interfaces
  //*********************************************** 
  public void init( GLAutoDrawable drawable) 
  {
    GL gl = drawable.getGL();
    gl.glClearColor( 0.0f, 0.0f, 0.0f, 0.0f);

    gl.glEnable( GL.GL_DEPTH_TEST );
    gl.glEnable( GL.GL_NORMALIZE );
    gl.glShadeModel( GL.GL_SMOOTH );

    camera.setCamera( camera.viewportWidth, camera.viewportHeight, gl );
    
    lights.setLights( gl );
  }

  // Redisplaying graphics
  public void display(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();

    /* clear the display */
    gl.glClear( GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT );

	gl.glPushMatrix();
	gl.glTranslatef(0f,0f,0.001f);
	draw_axes(4.5f, gl);
	gl.glPopMatrix();

    objects.render( gl, materials );

  }

  // Window size change
  public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h)
  {
    // Change viewport dimensions
    GL gl = drawable.getGL();
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

    switch ( key.getKeyChar() ) {
        case 'Q' :
        case 'q' : new Thread() {
                     public void run()
                     { animator.stop(); }
                   }.start();
                   System.exit(0);
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
  }

  public void mouseReleased(MouseEvent mouse)
  {
  }

  public void mouseMoved( MouseEvent mouse)
  {
  }

  public void mouseDragged( MouseEvent mouse )
  {
  }

  public void mouseEntered( MouseEvent mouse)
  {
  }

  public void mouseExited( MouseEvent mouse)
  {
  } 


}


