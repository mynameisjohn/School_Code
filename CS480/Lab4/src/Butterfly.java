//************************************************************************
//  Buttefly Class.
//     This class animates a buttefly.
//************************************************************************
//

import com.sun.opengl.util.*;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.util.*;
import java.io.IOException;

public class Butterfly 
{
  private int butterfly_obj, left_wing, right_wing;
  private int texture;
  private float x,y,z; // position
  private GLU glu = new GLU();

  public Butterfly( )
  {
    x = y = z = 0;
    butterfly_obj = left_wing = right_wing = 0;
  }

  public void init( GL gl )
  {
    try {
        texture = load_texture( gl, "Butterfly1.bmp" );
    } catch (IOException e) {
        System.out.println("Unable to load textures,Bailing!");
        throw new RuntimeException(e);
    } 

    create_left_wing( gl, texture );
    create_right_wing( gl, texture );

    butterfly_obj = gl.glGenLists(1);

    gl.glNewList( butterfly_obj, GL.GL_COMPILE );
    construct_disp_list( gl ); 
    gl.glEndList();
  }

  public void update( GL gl )
  {
    gl.glNewList( butterfly_obj, GL.GL_COMPILE );
    construct_disp_list( gl ); 
    gl.glEndList();
  }

  public void draw( GL gl )
  {
    gl.glPushMatrix();
    gl.glCallList( butterfly_obj );
    gl.glPopMatrix();
  }

  /////////////////// /////////////////  Private Functions 
  private void construct_disp_list( GL gl)
  { 
    gl.glBindTexture(GL.GL_TEXTURE_2D, texture);
    gl.glCallList( left_wing );
    gl.glCallList( right_wing );
  }

  private void flap_wings()
  {
  }

  private void translate()
  {
  }

  private int load_texture( GL gl, String filename) 
    throws IOException 
  {
    TextureReader.Texture texture =TextureReader.readTexture( filename );
    int[] texture_id = new int[1];
    gl.glGenTextures(1, texture_id, 0);

    //Create MipMapped Texture (Only with GL4Java 2.1.2.1 and later!)
    gl.glPixelStorei( GL.GL_UNPACK_ALIGNMENT, 4 );
    gl.glBindTexture(GL.GL_TEXTURE_2D, texture_id[0]);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, 
                       GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, 
                       GL.GL_LINEAR_MIPMAP_NEAREST);

    glu.gluBuild2DMipmaps(GL.GL_TEXTURE_2D,
                          3,
                          texture.getWidth(),
                          texture.getHeight(),
                          GL.GL_RGB,
                          GL.GL_UNSIGNED_BYTE,
                          texture.getPixels());

    return texture_id[0];
  }

  private void create_left_wing( GL gl, int texture)
  {
    left_wing = gl.glGenLists(1);
    
    gl.glNewList( left_wing, GL.GL_COMPILE);
    gl.glPushMatrix();
    gl.glEnable( GL.GL_TEXTURE_2D);
    gl.glPolygonMode( GL.GL_FRONT, GL.GL_FILL);
    gl.glPolygonMode( GL.GL_BACK, GL.GL_FILL);
    gl.glScalef(0.5f,0.5f,0.5f);
    gl.glTranslatef(-0.5f, -0.5f, 0);
    gl.glBegin( GL.GL_TRIANGLES );	  
      gl.glTexCoord2f(1.0f,1.0f); 
      gl.glVertex3f( 1.0f, 1.0f, 0.0f);
      gl.glTexCoord2f(0.0f,1.0f); 
      gl.glVertex3f( 0.0f, 1.0f, 0.0f);
      gl.glTexCoord2f(0.0f,0.0f); 
      gl.glVertex3f( 0.0f, 0.0f, 0.0f);
    gl.glEnd();
    gl.glDisable( GL.GL_TEXTURE_2D );
    gl.glPopMatrix();
    gl.glEndList();
  }

  private void create_right_wing( GL gl, int texture )
  {
    right_wing = gl.glGenLists(1);

    gl.glNewList( right_wing, GL.GL_COMPILE);
    gl.glPushMatrix();
    gl.glEnable( GL.GL_TEXTURE_2D );
    gl.glPolygonMode( GL.GL_FRONT, GL.GL_FILL);
    gl.glPolygonMode( GL.GL_BACK, GL.GL_FILL);
    gl.glScalef(0.5f,0.5f,0.5f);
    gl.glTranslatef(-0.5f, -0.5f, 0);
    gl.glBegin( GL.GL_TRIANGLES );
      gl.glTexCoord2f(1.0f,1.0f); 
      gl.glVertex3f( 1.0f, 1.0f, 0.0f);
      gl.glTexCoord2f(0.0f,0.0f); 
      gl.glVertex3f( 0.0f, 0.0f, 0.0f);
      gl.glTexCoord2f(1.0f,0.0f); 
      gl.glVertex3f( 1.0f, 0.0f, 0.0f);
    gl.glEnd();
    gl.glDisable( GL.GL_TEXTURE_2D );
    gl.glPopMatrix();
    gl.glEndList();
  }

}
