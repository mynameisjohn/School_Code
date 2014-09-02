//************************************************************************
//  Teapot Class.
//     This class draws a spinning teapot.
//************************************************************************
//

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.util.*;

public class Teapot
{
  private int teapot_object;
  private float scale;
  private float angle;

  public Teapot( float scale_)
  {
    scale = scale_;
    angle = 0;
  }

  public void init( GL gl )
  {

    teapot_object = gl.glGenLists(1);
    gl.glNewList( teapot_object, GL.GL_COMPILE );

    // due to a bug in glutSolidTeapot, triangle vertices are in CW order 
    gl.glFrontFace( GL.GL_CW ); 
  
    // create the teapot triangles 
    GLUT glut = new GLUT();
    glut.glutSolidTeapot( scale );
  
    // return to regular CCW order 
    gl.glFrontFace( GL.GL_CCW );
    gl.glEndList();

  }

  public void update( GL gl )
  {
    angle += 5;
  }

  public void draw( GL gl )
  {
    gl.glPushMatrix();
    gl.glPushAttrib( GL.GL_CURRENT_BIT );
    gl.glRotatef( angle, 0.0f, 1.0f, 0.0f );
    gl.glColor3f( 0.85f, 0.55f, 0.20f); // Orange
    gl.glCallList( teapot_object );
    gl.glPopAttrib();
    gl.glPopMatrix();
  }
}
