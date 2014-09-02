/* Program which solves the time dependent Schroedinger equation and 
   displays an animation of the wave function (simplified version).

                                            Claudio Rebbi
                                            Boston University
                                            February 1998        */

#include <stdio.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#define HALFLENGHT 25.0
#define HEIGHT 5.0
#define NXG 512

float xgr[NXG+1],vgr[NXG+1],psigr[NXG+1],psic[NXG+1][3];

extern void wfsetup_(float [NXG+1], float [NXG+1]);
extern void wfevolve_(float [NXG+1],float [NXG+1][3]);

void display(void)
{
  int i;
  glClear(GL_COLOR_BUFFER_BIT);
  wfevolve_(psigr,psic); 
  // wfevolve1s_(psigr,psic); 
  glBegin(GL_LINE_STRIP);
  for(i=0;i<=NXG;i++){
    glColor3fv(psic[i]);
    glVertex2f(xgr[i],psigr[i]);
  }
  glEnd();
  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 0.8, 1.0);
  for(i=0;i<=NXG;i++){
    glVertex2f(xgr[i],vgr[i]);
  }
  glEnd();
  glFlush(); 
  glutSwapBuffers();
}

void reshape(int w, int h)
{
  glViewport(0, 0, w, h);
}

main(int argc, char *argv[])
{
  wfsetup_(xgr,vgr);
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(1024,500);
  glutInitWindowPosition(50,50);
  glutCreateWindow("Wave function");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(-HALFLENGHT, HALFLENGHT, -HEIGHT, HEIGHT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glLineWidth(2.0);
  glutReshapeFunc(reshape);
  glutIdleFunc(display); 
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;   
}
