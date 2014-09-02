//****************************************************************************
// Cube class.  
//****************************************************************************
// Comments : 
//   Support Routines for a Textured cube class.
//     
// History  :
//   06 Feb 08 Created by Tai-Peng Tian (tiantp@gmail.com) based on Nehe
//   lesson08 port to JOGL by Pepijn Van Eeckhoudt downloaded from 
//   http://pepijn.fab4.be/software/nehe-java-ports/  
//


import com.sun.opengl.util.GLUT;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.util.*;

import java.io.IOException;

public class Cube
{
  private int[] textures = new int[3]; // Storage For 3 Textures
  private int filter; // Which texture to use
  private GLU glu = new GLU();
  private int cube_dl;
  private int draw_choice;

  public Cube() 
  {
    filter = 0; 
    cube_dl = 0;
    draw_choice = 1;
  }

  public void init( GL gl )
  {
    gl.glEnable( GL.GL_TEXTURE_2D);
    try {
        loadGLTextures( gl );
    } catch (IOException e) {
        System.out.println("Unable to load textures,Bailing!");
        throw new RuntimeException(e);
    } 

    // create the display list
    cube_dl = gl.glGenLists(1);
    gl.glNewList( cube_dl, GL.GL_COMPILE );
    draw_internal( gl );
    gl.glEndList();
  }

  public void set_choice( int choice )
  {
    draw_choice = choice; 
  }


  public void draw( GL gl )
  {
    switch (draw_choice)
    {
      case 1 : gl.glCallList( cube_dl );
               break;

      case 2 : draw_2( gl );
               break;
     
      case 3 : draw_3( gl );
               break;

      case 4 : draw_4( gl );
               break;

      default : gl.glCallList( cube_dl );
               break;
    }
  }

  private void draw_2( GL gl )
  {
    gl.glPushMatrix();
    gl.glTranslatef( -1, 0, 0);
    gl.glCallList( cube_dl );
    gl.glPopMatrix();

    gl.glPushMatrix();
    gl.glTranslatef(  1, 0, 0);
    gl.glCallList( cube_dl );
    gl.glPopMatrix();
  }

  private void draw_unit_cube( GL gl )
  {
    gl.glPushMatrix();
    gl.glScaled( 0.5, 0.5, 0.5 );
    gl.glCallList( cube_dl );
    gl.glPopMatrix();
  }
   
  private void draw_small( GL gl )
  {
    gl.glPushMatrix();
    gl.glScaled( 0.2, 0.2, 0.2 );
    gl.glCallList( cube_dl );
    gl.glPopMatrix();
  }

  private void draw_3( GL gl )
  {
    gl.glPushMatrix();

    gl.glTranslatef( -2, -2, 0);
    draw_unit_cube( gl );
    
    gl.glTranslatef(  1,  1, 0);
    gl.glPushMatrix();
       gl.glRotatef(-90, 0, 0, 1);
       draw_unit_cube( gl );
    gl.glPopMatrix();

    
    gl.glTranslatef(  1,  1, 0);
    gl.glPushMatrix();
       gl.glRotatef(-180, 0, 0, 1);
       draw_unit_cube( gl );
    gl.glPopMatrix();


    gl.glTranslatef(  1,  1, 0);
    gl.glPushMatrix();
       gl.glRotatef(-270, 0, 0, 1);
       draw_unit_cube( gl );
    gl.glPopMatrix();

    gl.glPopMatrix();
  }

  private void draw_4( GL gl )
  {
    gl.glPushMatrix();
    for (int i = 0; i<3; i++)
    {
      gl.glTranslatef( 0, 0.5f, 0);
      draw_small( gl );
    }
    gl.glPopMatrix();

    gl.glPushMatrix();
    
    for (int i = 0; i < 6; ++i)
    {
      gl.glRotatef( 60, 0, 1, 0);

      // draw a branch
      gl.glPushMatrix();
      for (int j = 0; j<3; j++)
      {
        gl.glTranslatef( 0.5f, 0, 0);
        draw_small( gl );
      }
      gl.glPopMatrix();
    }

    gl.glPopMatrix();
  }

  private void draw_internal( GL gl )
  {
     gl.glPushMatrix();

     gl.glBindTexture(GL.GL_TEXTURE_2D, textures[filter]);

     gl.glBegin(GL.GL_QUADS);
     // Front Face
     gl.glNormal3f(0.0f, 0.0f, 1.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(-1.0f, -1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(1.0f, -1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(1.0f, 1.0f, 1.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(-1.0f, 1.0f, 1.0f);
     // Back Face
     gl.glNormal3f(0.0f, 0.0f, -1.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(-1.0f, -1.0f, -1.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(-1.0f, 1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(1.0f, 1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(1.0f, -1.0f, -1.0f);
     // Top Face
     gl.glNormal3f(0.0f, 1.0f, 0.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(-1.0f, 1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(-1.0f, 1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(1.0f, 1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(1.0f, 1.0f, -1.0f);
     // Bottom Face
     gl.glNormal3f(0.0f, -1.0f, 0.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(-1.0f, -1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(1.0f, -1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(1.0f, -1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(-1.0f, -1.0f, 1.0f);
     // Right face
     gl.glNormal3f(1.0f, 0.0f, 0.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(1.0f, -1.0f, -1.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(1.0f, 1.0f, -1.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(1.0f, 1.0f, 1.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(1.0f, -1.0f, 1.0f);
     // Left Face
     gl.glNormal3f(-1.0f, 0.0f, 0.0f);
     gl.glTexCoord2f(0.0f, 0.0f);
     gl.glVertex3f(-1.0f, -1.0f, -1.0f);
     gl.glTexCoord2f(1.0f, 0.0f);
     gl.glVertex3f(-1.0f, -1.0f, 1.0f);
     gl.glTexCoord2f(1.0f, 1.0f);
     gl.glVertex3f(-1.0f, 1.0f, 1.0f);
     gl.glTexCoord2f(0.0f, 1.0f);
     gl.glVertex3f(-1.0f, 1.0f, -1.0f);
     gl.glEnd();

     gl.glPopMatrix();
  }

  private void loadGLTextures( GL gl ) 
    throws IOException {
      TextureReader.Texture texture = TextureReader.readTexture("glass.png");

      //Create Nearest Filtered Texture
      gl.glGenTextures(3, textures, 0);
      gl.glBindTexture(GL.GL_TEXTURE_2D, textures[0]);

      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MAG_FILTER, GL.GL_NEAREST);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MIN_FILTER, GL.GL_NEAREST);

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
              0,
              3,
              texture.getWidth(),
              texture.getHeight(),
              0,
              GL.GL_RGB,
              GL.GL_UNSIGNED_BYTE,
              texture.getPixels());

      //Create Linear Filtered Texture
      gl.glBindTexture(GL.GL_TEXTURE_2D, textures[1]);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
              0,
              3,
              texture.getWidth(),
              texture.getHeight(),
              0,
              GL.GL_RGB,
              GL.GL_UNSIGNED_BYTE,
              texture.getPixels());

      //Create MipMapped Texture (Only with GL4Java 2.1.2.1 and later!)
      gl.glBindTexture(GL.GL_TEXTURE_2D, textures[2]);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, 
          GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR_MIPMAP_NEAREST);

      glu.gluBuild2DMipmaps(GL.GL_TEXTURE_2D,
              3,
              texture.getWidth(),
              texture.getHeight(),
              GL.GL_RGB,
              GL.GL_UNSIGNED_BYTE,
              texture.getPixels());
  }
}
