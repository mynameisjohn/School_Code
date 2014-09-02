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
#include <math.h>

#define LENGTH 4.0
#define HEIGHT 2.0
#define N 35

int step=0, k, s;
float dphi;

void display(void)
{
  int i;
  float th, x;
  static float phi=0;

  glClear(GL_COLOR_BUFFER_BIT);
  glPushMatrix();  
  glTranslatef (0.0, 0.0, -10.0);
  glRotatef(phi,0,1,0);  
  //  glRotatef(30,0,1,0); 
  glRotatef(20,1,0,0); 
  
  glLineWidth(1.0);
  glColor3f(0.0, 0.5, 1.0);
  glBegin(GL_LINE_STRIP);
  glVertex3f(-LENGTH, 0.0, 0.0);
  glVertex3f(LENGTH, 0.0, 0.0);
  glEnd();

  glBegin(GL_LINE_STRIP);
  glVertex3f(-LENGTH, HEIGHT, 0.0);
  glVertex3f(-LENGTH, 0.0, 0.0);
  glVertex3f(-LENGTH, 0.0, HEIGHT);
  glEnd();

  if(step==1){

    dphi=0.01;

    glLineWidth(1.0);
    glColor3f(1.0, 0.0, 0.0);
    glBegin(GL_LINE_STRIP);
    for(i=0;i<N;i++){
      th=(2*M_PI*i*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
    }
    glEnd();      
      
    glLineWidth(2.0);
    glColor3f(1.0, 0.0, 1.0);
  
    for(i=0;i<N;i++){
      th=(2*M_PI*i*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glBegin(GL_LINE_STRIP);
      glVertex3f(x, 0.0, 0.0);
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
      glEnd();      
    }
  }

  if(step==2){

    glLineWidth(1.0);
    glColor3f(1.0, 0.0, 0.0);
    glBegin(GL_LINE_STRIP);
    for(i=0;i<N;i++){
      th=(2*M_PI*i*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
    }
    glEnd();      
      
    glLineWidth(1.0);
    glColor3f(1.0, 0.0, 1.0);
  
    for(i=0;i<N;i++){
      th=(2*M_PI*i*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glBegin(GL_LINE_STRIP);
      glVertex3f(x, 0.0, 0.0);
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
      glEnd();      
    }

    glLineWidth(1.0);
    glColor3f(1.0, 0.8, 0.0);
    glBegin(GL_LINE_STRIP);
    for(i=0;i<N;i++){
      th=(2*M_PI*(i+s)*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
    }
    glEnd();      
      
    glLineWidth(2.0);
    glColor3f(0.0, 1.0, 0.0);
  
    for(i=0;i<N;i++){
      th=(2*M_PI*(i+s)*k)/N;
      x=-LENGTH+(2.*LENGTH*i)/N+0.01*LENGTH;
      glBegin(GL_LINE_STRIP);
      glVertex3f(x, 0.0, 0.0);
      glVertex3f(x, 0.5*HEIGHT*sin(th), 0.5*HEIGHT*cos(th));
      glEnd();      
    }
  }

  glFlush(); 
  glutSwapBuffers();
  glPopMatrix(); 

  if(step<2) phi=0;
  if(step==2) phi+=dphi;

}

void reshape(int w, int h)
{
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective (42.0, (GLdouble)w/(GLdouble)h, 1.0, 20.0); 
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

void choice(int menuitem)
{
  switch(menuitem){
  case 1:
    printf("Enter the eigenvector index: "); 
    scanf("%d",&k); 
    step=1; 
    break;
  case 2:
    if(step==0){
      printf("You must specify the eigenvector index first.\n");
      break;
    }
    printf("Enter the shift: ");
    scanf("%d",&s);  
    step=2; 
    break;
  case 4:
    dphi=0;
    break;
  case 5:
    dphi=0.01;
    break;
  case 3:
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
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(800,500);
  glutInitWindowPosition(50,50);
  glutCreateWindow("Cshift eigenvectors");
  printf("\nClick on the window with the left mouse button to call a menu ...\n\n");
  glClearColor (0.0, 0.0, 0.0, 1.0);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  glutIdleFunc(display);  
  glutCreateMenu(choice);
  glutAddMenuEntry("choose k", 1);
  glutAddMenuEntry("shift", 2);
  glutAddMenuEntry("stop", 4);
  glutAddMenuEntry("rotate", 5);
  glutAddMenuEntry("exit", 3);
  glutAttachMenu(GLUT_LEFT_BUTTON);
  glutMainLoop();
  return 0;   
}
