/* This is the graphics driver code for the program which implements
   the blurring of an image.  It calls the fortran subroutine blur_
   where the blurring, equivalent to a diffusion process, is done.

                            Copyright by Claudio Rebbi, January 2012 
*/
   

#include <stdio.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#define WIDTH 500

int xwidth=WIDTH, ywidth=WIDTH;
int xw[1], yw[1], pxa[250000];

extern void blur_(int *, int *, int *);

void display(void)
{
  float rp[2];

  blur_(xw, yw, pxa);

  glClear(GL_COLOR_BUFFER_BIT);

  rp[0]=-((float)*xw)/xwidth; rp[1]=-((float)*yw)/ywidth;
  glRasterPos2fv(rp);
  glDrawPixels(*xw, *yw, GL_RGBA, GL_UNSIGNED_BYTE, pxa);

  glFlush(); 
  glutSwapBuffers();
}

void reshape(int w, int h)
{
  xwidth=w;
  ywidth=h;
  glViewport(0, 0, w, h);
}

main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(500,500);
  glutInitWindowPosition(50,50);
  glutCreateWindow("Image diffusion");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glutReshapeFunc(reshape);
  glutIdleFunc(display); 
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;   
}
