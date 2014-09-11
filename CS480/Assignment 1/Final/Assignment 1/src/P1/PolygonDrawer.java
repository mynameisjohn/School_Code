/**
 * PolygonDrawer.java
 */
package P1;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;

/**
 * A class which provides a single public method which draws a polygon.
 * 
 * This method contains separate internal methods for drawing a convex polygon
 * and for drawing a concave polygon.
 * 
 * @author Jeffrey Finkelstein <jeffreyf>
 * @since Spring 2011
 */
public class PolygonDrawer {

	/**
	 * Draws the specified polygon (concave or convex) on the specified OpenGL
	 * drawable object.
	 * 
	 * @param drawable
	 *          The OpenGL object on which to draw this polygon.
	 * @param polygon
	 *          The polygon to draw.
	 */
	public static void draw(final GLAutoDrawable drawable, final Polygon polygon) {
		 if (polygon.isConcave()) {
		drawConcave(drawable, polygon);
		 } else {
		   drawConvex(drawable, polygon);
		 }
	}

	/**
	 * Draws the specified concave polygon with the specified OpenGL drawable
	 * object.
	 * 
	 * Pre-condition: the specified polygon is concave.
	 * 
	 * @param drawable
	 *          The OpenGL object on which to draw this polygon.
	 * @param polygon
	 *          The polygon to draw.
	 */
	private static void drawConcave(final GLAutoDrawable drawable,
			final Polygon polygon) {

		final GL gl = drawable.getGL();


		/**
		 * Your code for drawing a concave polygon, using the openGL stencil buffer should go here.
		 */

		//Thus begins my stencil test code. I've worked through the logic and I think I have a good understanding of how it works. 
		gl.glEnable(GL.GL_STENCIL_TEST);
		gl.glClearStencil(0);//Clearing the stencil buffer. 
		gl.glColorMask(false, false, false, false);//Masking color bits so they are not affected. 
		gl.glStencilFunc(GL.GL_NEVER, 0, 1);//Set the test to always fail. 
		gl.glStencilOp(GL.GL_INVERT, GL.GL_INVERT, GL.GL_INVERT);//No matter what the bits are inverted, and the last two arguments shouldn't come up anyway

		//When we draw here we flip every bit touched by a triangle in the stencil buffer.
		//Since we are inverting, bits that get drawn twice end up with a zero and are not drawn
		gl.glBegin(GL.GL_TRIANGLE_FAN);//Draw the polygon using the triangle fan method
		gl.glVertex2f(polygon.vertices.get(0).x, polygon.vertices.get(0).y);//First vertex
		for (int i=1;i<polygon.vertices.size()-1;i++)
		{//Correct vertices are added as needed to the triangle fan
			gl.glVertex2f(polygon.vertices.get(i).x, polygon.vertices.get(i).y);
			gl.glVertex2f(polygon.vertices.get(i+1).x, polygon.vertices.get(i+1).y);
		}
		gl.glEnd();
		
		gl.glColorMask(true, true, true, true);//Unmask the color bits
		gl.glStencilFunc(GL.GL_EQUAL, 1, 1);
		gl.glStencilOp(GL.GL_ZERO, GL.GL_ZERO,GL.GL_ZERO);
		//I don't understand this part, but it works. How does openGL know what was 1 if I zero it here? Maybe it is part of the Stencil Test
		drawConvex(drawable, polygon);
		
		gl.glDisable(GL.GL_STENCIL_TEST);
		
		/**
		 * The next line is only here until you implement your own algorithm for drawing
		 * a concave polygon using the OpenGL stencil buffer technique described in class. 
		 */

		//drawConvex(drawable, polygon);
	}

	/**
	 * Draws the specified convex polygon with the specified OpenGL drawable
	 * object.
	 * 
	 * Pre-condition: the specified polygon is convex.
	 * 
	 * @param drawable
	 *          The OpenGL object on which to draw this polygon.
	 * @param polygon
	 *          The polygon to draw.
	 */
	public static void drawConvex(final GLAutoDrawable drawable,
			final Polygon polygon) {
		if (polygon.vertices().isEmpty())
			return;

		final GL gl = drawable.getGL();

		// push the current color
		gl.glPushAttrib(GL.GL_CURRENT_BIT);

		// use the current polygon's color
		Color color = polygon.getColor();
		gl.glColor3f(color.getRed(), color.getGreen(), color.getBlue());

		// if only 1 vertex, draw a point
		if (polygon.vertices().size() == 1)
			gl.glBegin(GL.GL_POINTS);

		// if only 2 vertices, draw a line
		else if (polygon.vertices().size() == 2)
			gl.glBegin(GL.GL_LINES);

		// otherwise draw a polygon
		else
			gl.glBegin(GL.GL_POLYGON);

		for (final Point vertex : polygon.vertices())
			gl.glVertex2f(vertex.x, vertex.y);

		gl.glEnd();

		// pop current color
		gl.glPopAttrib();
	}
}
