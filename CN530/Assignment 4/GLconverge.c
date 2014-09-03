/* 
   plotter.c
   A simple c program that plots a network of N neurons 
   using the Distance-Dependent Shunting model. I chose to 
   use wrap-around boundary conditions. A special thanks
   to Ben Alpert for the timing code. 

   John Joseph
   10/5/13
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "GL/freeglut.h"
#include "GL/gl.h"

#define DT 0.0005
#define T_MAX 10
#define N 20

//The data Type used for simulation
typedef double T;

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

T sigFunc(T F, T w)
{
  //Linear
  //return w;

  //Faster than linear
  //return w*w;

  //Slower than linear
  // return w/(F+w);

  //Sigmoidal
  return (w*w)/(F+w*w);
}

// I changed my RK method to solve any 
// differential equation of the form
// dx/dt = -ax+b
T solveRK (T x0, T a, T b)
{
  T k1,k2,k3,k4,s;
  
  k1 = a*x0+b;
  k2 = a*eulerAdvance(x0,k1,DT/2)+b;
  k3 = a*eulerAdvance(x0,k2,DT/2)+b;
  k4 = a*eulerAdvance(x0,k3,DT)+b;
  s = (k1+2*k2+2*k3+k4);
  return eulerAdvance(x0,s,DT/6.0);
}
T t=0;

T A=1, B=3, F=0.25;
T I[]={0.2,0.7,0.9,0.6,0.3,0.5,0.4,0.8,0.5,0.1}, *x;

char buffer [10];
/* 
void display(){
   static float 
}
 */
void renderFunction()
{
  T Xsum=0;
  int k,i;
  if (t<1) {

    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
	a=(-A-I[i%10]-Xsum);
	b=B*(sigFunc(F,x[i])+I[i%10]);
	x[i]=solveRK(x[i],a,b);
      }  
    //printf("wtf");
  
  }
  else if (t<T_MAX) {

    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
	//	printf("%lf\n",x[i]);
	a=(-A-Xsum);
	b=B*sigFunc(F,x[i]);
	x[i]=solveRK(x[i],a,b);

      }  
    
  }
  t+=DT;
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(1.0, 1.0, 1.0);
  glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
  glBegin(GL_LINES);
  for (i=0;i<N-1;i++){
    glVertex2f(((float)i)/50.0f-0.9, x[i]);
    glVertex2f(((float)(i+1))/50.0f-0.9, x[i+1]);
  }
  glEnd();
  glRasterPos2f(0.0f,-0.5f);
   glColor4f(0.0f,0.0f,1.0,1.0f);
   sprintf(buffer,"%6.2f",t);
   glutBitmapString(GLUT_BITMAP_HELVETICA_18, buffer);
  glFlush();
  //printf("%lf\n",t);
}
int main(int argc, char** argv)
{
  x=malloc(sizeof(T)*N);
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE);
  glutInitWindowSize(500,500);
  glutInitWindowPosition(100,100);
  glutCreateWindow("OpenGL - First window demo");
  glutDisplayFunc(renderFunction);
  glutIdleFunc(renderFunction);
  glutMainLoop();

   return 1;
}
