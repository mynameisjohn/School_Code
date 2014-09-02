/**
 * SolidCylinder.java - a cylinder which can draw itself using a triangle mesh
 * approximation in OpenGL
 * 
 * History:
 * 
 * 18 February 2011
 * 
 * - added documentation
 * 
 * (Jeffrey Finkelstein <jeffrey.finkelstein@gmail.com>)
 * 
 * 30 January 2008
 * 
 * - translated from C code by Stan Sclaroff
 * 
 * (Tai-Peng Tian <tiantp@gmail.com>)
 */


import java.util.ArrayList;

import javax.media.opengl.GL;

/**
 * A solid cylinder which provides methods for drawing using OpenGL.
 * 
 * @author Tai-Peng Tian <tiantp@gmail.com>
 * @since Spring 2008
 */
public class SolidCylinder {
  /** The points along the edges of the approximated circle. */
  private final ArrayList<Point3D> circle2D = new ArrayList<Point3D>();
  /** The points which are normal to the edges of the approximated circle. */
  private final ArrayList<Point3D> circle2D_normal = new ArrayList<Point3D>();
  /**
   * The OpenGL handle to the display list which contains all the components
   * which comprise this cylinder.
   */
  private int DL;
  /** The z component of the top face of the cylinder. */
  private final double z_top;
  /** The z component of the bottom face of the cylinder. */
  private final double z_bottom;

  /**
   * Instantiates this cylinder with center of bottom base at (tx, ty, tz) with
   * the specified radius and height.
   * 
   * The n_subdivision_steps parameter specifies the number of triangle
   * subdivisions to use when approximating the sides of the cylinder. A higher
   * number means more triangles and therefore a smoother cylinder.
   * 
   * @param tx
   *          The x component of the center of the bottom base of the cylinder.
   * @param ty
   *          The y component of the center of the bottom base of the cylinder.
   * @param tz
   *          The z component of the center of the bottom base of the cylinder.
   * @param radius
   *          The radius of the cylinder.
   * @param height
   *          The height of the cylinder.
   * @param n_subdivision_steps
   *          The number of triangle subdivisions to use when approximating the
   *          sides of the cylinder.
   */
  public SolidCylinder(final double tx, final double ty, final double tz,
      final double radius, final double height, final int n_subdivision_steps) {

    // cylinder is centered at (0,0,0) and aligned along Z-axis
    this.z_top = tz + height;
    this.z_bottom = tz;

    // Approximation of the 2D circle with polyline
    final double d_theta = 2 * 3.1415926 / n_subdivision_steps;
    for (double theta = 0; theta < 2 * Math.PI; theta += d_theta) {
      final Point3D normal = new Point3D(radius * Math.cos(theta), radius
          * Math.sin(theta), 0);
      this.circle2D_normal.add(normal);
      this.circle2D.add(new Point3D(normal.x() + tx, normal.y() + ty, 1));
      theta += d_theta;
    }

  }

  /**
   * Draws the cylinder by drawing the appropriate GL call list which contains
   * its component parts.
   * 
   * @param gl
   *          The OpenGL object with which to draw the cylinder.
   */
  public void draw(final GL gl) {
    gl.glCallList(this.DL);
  }

  /**
   * Initializes the GL call lists which make up the components of this cylinder
   * using the specified OpenGL object.
   * 
   * @param gl
   *          The OpenGL object on which to create the call lists which comprise
   *          this cylinder.
   */
  public void init(final GL gl) {
    this.DL = gl.glGenLists(1);

    gl.glNewList(this.DL, GL.GL_COMPILE);

    Point3D n, p; // temp variables to store retrieved obj from ArrayList

    // begin a triangle strip for the sides of the cylinder
    gl.glBegin(GL.GL_TRIANGLE_STRIP);
    for (int i = 0; i < this.circle2D.size(); i++) {
      n = this.circle2D_normal.get(i);
      p = this.circle2D.get(i);

      gl.glNormal3d(n.x(), n.y(), 0);
      gl.glVertex3d(p.x(), p.y(), this.z_bottom);
      gl.glNormal3d(n.x(), n.y(), 0);
      gl.glVertex3d(p.x(), p.y(), this.z_top);
    }

    n = this.circle2D_normal.get(0);
    p = this.circle2D.get(0);
    gl.glNormal3d(n.x(), n.y(), 0);
    gl.glVertex3d(p.x(), p.y(), this.z_bottom);
    gl.glNormal3d(n.x(), n.y(), 0);
    gl.glVertex3d(p.x(), p.y(), this.z_top);

    gl.glEnd(); // end the sides of the cylinder

    // begin a polygon which approximates the top of the cylinder
    gl.glBegin(GL.GL_POLYGON);
    for (int i = 0; i < this.circle2D.size(); i++) {
      p = this.circle2D.get(i);
      gl.glVertex3d(p.x(), p.y(), this.z_top);
      gl.glNormal3d(0, 0, 1);
    }
    p = this.circle2D.get(0);
    gl.glVertex3d(p.x(), p.y(), this.z_top);
    gl.glNormal3d(0, 0, 1);

    gl.glEnd(); // end the top of the cylinder

    // begin a polygon which approximates the bottom of the cylinder
    gl.glBegin(GL.GL_POLYGON);
    for (int i = 0; i < this.circle2D.size(); i++) {
      p = this.circle2D.get(i);
      gl.glVertex3d(p.x(), p.y(), this.z_bottom);
      gl.glNormal3d(0, 0, 1);
    }
    p = this.circle2D.get(0);
    gl.glVertex3d(p.x(), p.y(), this.z_bottom);
    gl.glNormal3d(0, 0, 1);

    gl.glEnd(); // end the bottom of the cylinder

    gl.glEndList();
  }
}
