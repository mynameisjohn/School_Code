//****************************************************************************
// Polygon class.  
//****************************************************************************
// Comments : 
//   Subroutines to manage and draw polygons
//
// History :
//   9 Jan 2008 Created by Tai-Peng Tian (tiantp@gmail.com) based on code by
//   Stan Sclaroff (from CS480 '06 poly.c)

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.util.*;

public class Polygon 
{

  private class coord
  {
    public float x, y;
    public coord(float _x, float _y)
    {
      x = _x;
      y = _y;
    }
    public void print()
    {
      System.out.print(x+", "+y);
    }
  }

  private int selected_vert;
  private boolean is_concave;
  private ArrayList<coord> verts;
  private ColorType color;
  private GLU glu;

  public Polygon()
  {
    // Initialization for the data structure
    color = new ColorType( 0f, 0f, 0f);
    verts = new ArrayList<coord>();
    glu = new GLU();

    is_concave = false;
    selected_vert = -1;
    changeColor(1.0f,1.0f,1.0f);
  }

  public void reset()
  {
    is_concave = false;
    selected_vert = -1;
    verts.clear();
  }

  public void print()
  {
    for (int i=0; i < verts.size(); i++)
    {
      coord c = verts.get(i);
      c.print();
      System.out.println();
    }
  }

  // change color for drawing the outline of the polygon
  public void changeColor(float r, float g, float b)
  {
    color.r = r;
    color.g = g;
    color.b = b;
  }

  // Generate necessary OpenGL commands to draw a convex polygon
  public void drawConvexPoly( GLAutoDrawable drawable )
  {
    int i;
   
    GL gl = drawable.getGL();

    if ( verts.size() == 0)
      return;

    /* push the current color */
    gl.glPushAttrib(GL.GL_CURRENT_BIT);

    /* set boundary color */
    gl.glColor3f( color.r, color.g, color.b);

    /* if only 1 vert, draw a point */
    if (verts.size() == 1)
      gl.glBegin(GL.GL_POINTS);

    /* if only 2 verts, draw a line */
    else if (verts.size() == 2)
      gl.glBegin(GL.GL_LINES);

    /* otherwise draw a polygon */
    else 
      gl.glBegin(GL.GL_POLYGON);

    Iterator itr = verts.iterator();
    while (itr.hasNext())
    {
      coord c = (coord) itr.next();
      gl.glVertex2f(c.x, c.y);
    }

    gl.glEnd();

    /* pop current color */
    gl.glPopAttrib();

  }

  // move selected vertex to a  new position
  public void moveVert( int x, int y )
  {
    // Ensure there is at least 1 vertex and the selected vertex is not out of
    // bounds.
    if (selected_vert >= 0 && selected_vert < verts.size())
    {
      coord c = verts.get(selected_vert); 
      c.x = x;
      c.y = y;
    }
  }

  // add vertex to polygon
  public void addVert( int x, int y )
  {
    verts.add( new coord(x,y));
  }

  // select a vertex based on a mouse click position (x,y)
  public void selectVert( int x, int y)
  {
    int i, winner;
    float dist_squared, winning_dist_squared, dx, dy;

    if (verts.size() == 0) // polyon is empty
      return;

    coord c = verts.get(0);
    dx = x - c.x;
    dy = y - c.y;
    winning_dist_squared = dx*dx + dy*dy;
    winner = 0;

    for (i=1; i<verts.size(); ++i)
    {
      c = verts.get(i);
      dx = x - c.x;
      dy = y - c.y;
      dist_squared = dx*dx + dy*dy;
      if (dist_squared < winning_dist_squared)
      {
        winner = i;
        winning_dist_squared = dist_squared;
      }
    }

    selected_vert = winner;

  }

  //
  //  Functions to specified for this Assignment
  //
  //

  // Determines if point (x,y) is within the convex polygon
  public boolean insidePoly( int x, int y)
  {
    /* For now -- always returns FALSE */
    /* remove it when you write your own inside outside test */
    return false;

    /* your code goes here */
  }

  // Determines if the polygon is convex or concave
  public boolean concavePoly()
  {
    /* For now -- always returns FALSE */
    return false;
  }

  // Draws a convcave polygon
  public void drawConcavePoly( GLAutoDrawable drawable )
  {
    GL gl = drawable.getGL();

    /* this is only here until you write your concave handler */
    /* comment it out once you do! */
    drawConvexPoly( drawable );

    /* your code for subdividing/drawing a concave polygon here */
  }
}
























