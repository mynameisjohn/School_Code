/*  This program simulates and visualizes the Euclidean path integral for
    a particle of mass=1 in a potential lambda*(x**2-1)**2.  The system is
    defined with periodic boundary conditions in time for an evolution
    of duration t (corresponding therefore to a temperature T=1/t, with 
    hbar=1).  The program simulates the system, plots the configurations,
    the distribution of the particle coordinate and the logarithm of the 
    correlation function <x(t0)*x(t1)>.  It also calculates the exact 
    wavefunctions for the two lowest energy levels and plots their squared 
    modulus together with the corresponding rate of decay in Euclidean time.

                                                           Copyright by
                                                           Claudio Rebbi
                                                           December 1992
                                                           modified in
							   April 1998
*/

#include <stdio.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#define N 50   
#define NHALF 25   
#define NHIS 20

int window1,window2,window3;
float tg[N+1],xg[N+1];
float xd[2*NHIS+1],fe[2*NHIS+1],fo[2*NHIS+1],fd[2*NHIS+1];
float tc[NHALF+1],ce[NHALF+1],cd[NHALF+1];

extern void mcsetup_(float [N+1], float [2*NHIS+1], float[2*NHIS+1], \
		     float [2*NHIS+1], float[NHALF+1], float[NHALF+1]);
extern void mc_(float [N+1]);
extern void measure_(float [2*NHIS+1], float[NHALF+1]);
extern void mcstart_(void);
extern void mcstop_(void);

void display3(void)
{
  int i;
  glClear(GL_COLOR_BUFFER_BIT);

  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 1.0, 0.0);
  for(i=0;i<=NHALF;i++){
    glVertex2f(tc[i],ce[i]);
  }
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 1.0, 1.0);
  for(i=0;i<=NHALF;i++){
    glVertex2f(tc[i],cd[i]);
  }
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.1,0.05);
  glVertex2f(0.45,0.05);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.1,0.05);
  glVertex2f(0.1,0.5);
  glEnd();

  glFlush(); 
  glutSwapBuffers();

  glutSetWindow(window1);
}

void display2(void)
{
  int i;
  glClear(GL_COLOR_BUFFER_BIT);

  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 1.0, 0.0);
  for(i=0;i<=2*NHIS;i++){
    glVertex2f(xd[i],fe[i]);
  }
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 0.0, 1.0);
  for(i=0;i<=2*NHIS;i++){
    glVertex2f(xd[i],fo[i]);
  }
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(0.0, 1.0, 1.0);
  for(i=0;i<=2*NHIS;i++){
    glVertex2f(xd[i],fd[i]);
  }
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.5,0.05);
  glVertex2f(0.9,0.05);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 1.0, 1.0);
  glVertex2f(0.7,0.05);
  glVertex2f(0.7,0.5);
  glEnd();

  glFlush(); 
  glutSwapBuffers();

  glutSetWindow(window3); 
  display3();
}

void display1(void)
{
  int i;
  static int j=0;

  glClear(GL_COLOR_BUFFER_BIT);

  mc_(xg);
  measure_(fd,cd);

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.1,0.75);
  glVertex2f(0.9,0.75);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 1.0, 1.0);
  glVertex2f(0.1,0.65);
  glVertex2f(0.9,0.65);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 1.0, 1.0);
  glVertex2f(0.1,0.85);
  glVertex2f(0.9,0.85);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glColor3f(1.0, 1.0, 0.0);
  for(i=0;i<=N;i++){
    glVertex2f(tg[i],xg[i]);
  }
  glEnd();

  glFlush(); 
  glutSwapBuffers();

  j++;

  if(j==10){
    glutSetWindow(window2); 
    display2();
    j=0;
  }
}

void reshape(int w, int h)
{
  glViewport(0, 0, w, h);
}

void choice(int menuitem)
{
  switch(menuitem){
  case 1:
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush(); 
    glutSwapBuffers();
    glutSetWindow(window2); 
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush(); 
    glutSwapBuffers();
    glutSetWindow(window3); 
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush(); 
    glutSwapBuffers();
    glutSetWindow(window1); 
    mcsetup_(tg,xd,fe,fo,tc,ce);
    break;
  case 2:
    mcstart_();
    break;
  case 3:
    mcstop_();
    break;
  case 4:
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush(); 
    glutSwapBuffers();
    printf("\n");
    exit(0);
    break;
  default:
    exit(0);
  }
}

main(int argc, char *argv[])
{
  printf("\nClick on the main window with the left mouse button to call a menu ...\n");
  mcsetup_(tg,xd,fe,fo,tc,ce); 
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

  glutInitWindowSize(800,400);
  glutInitWindowPosition(50,50);
  window1=glutCreateWindow("Trajectory");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0.0, 1.0, 0.5, 1.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glLineWidth(1.0);
  glutReshapeFunc(reshape);
  glutDisplayFunc(display1);
  glutCreateMenu(choice);
  glutAddMenuEntry("setup", 1);
  glutAddMenuEntry("start", 2);
  glutAddMenuEntry("stop", 3);
  glutAddMenuEntry("exit", 4);
  glutAttachMenu(GLUT_LEFT_BUTTON);

  glutInitWindowSize(300,400);
  glutInitWindowPosition(865,50);
  window2=glutCreateWindow("Prob. density");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0.4, 1.0, 0.0, 0.8);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glLineWidth(1.0);
  glutReshapeFunc(reshape);
  glutDisplayFunc(display2);

  glutInitWindowSize(400,300);
  glutInitWindowPosition(765,487);
  window3=glutCreateWindow("Correlation");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0.0, 0.55, 0.0, 0.55);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glLineWidth(1.0);
  glutReshapeFunc(reshape);
  glutDisplayFunc(display3);

  glutSetWindow(window1);

  glutIdleFunc(display1); 

  glutMainLoop();
  return 0;   
}
