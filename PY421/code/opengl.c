/* Program which illustrates the use of the GL and glut libraries
   to open a window and display a simple figure. 

                                            Claudio Rebbi
                                            Boston University
                                            February 1998        */

#include <stdio.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#define LENGTH 20.0
#define HEIGHT 10.0

void display(void)
{
  static float theta=20;
  // static float theta=0;
  // static float dtheta=0.1; 
  glClear(GL_COLOR_BUFFER_BIT);
  // glPushMatrix();  
  // glRotatef(theta,0,0,1); 
  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(9.0, 0.0);
  glColor3f(1.0, 1.0, 0.0);
  glVertex2f(0.0, 3.0);
  glColor3f(0.0, 1.0, 0.0);
  glVertex2f(-9.0, 0.0);
  glColor3f(0.0, 0.0, 1.0);
  glVertex2f(0.0, -3.0);
  glColor3f(1.0, 0.0, 1.0);
  glVertex2f(9.0, 0.0);
  glEnd();
  glFlush(); 
  glutSwapBuffers();
  // glPopMatrix();  
  // theta+=dtheta; 
}

void reshape(int w, int h)
{
  glViewport(0, 0, w, h);
}

main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(1000,500);
  glutInitWindowPosition(50,50);
  glutCreateWindow(argv[0]);
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(-LENGTH, LENGTH, -HEIGHT, HEIGHT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glLineWidth(3.0);
  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  // glutIdleFunc(display);  
  glutMainLoop();
  return 0;   
}
