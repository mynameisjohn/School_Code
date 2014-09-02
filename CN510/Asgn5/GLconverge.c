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
#define N 10

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

void simulate(T A, T B, T F, T *I, T *x, char numTest)
{
  // naming the output file
  FILE *output; char *fileName = malloc(sizeof(char)*14);
  if (sizeof(T)==4)
    strcpy(fileName,"./data/32/dat_.out");
  else
    strcpy(fileName,"./data/64/dat_.out");
  fileName[13]=numTest;
  output=fopen(fileName,"w");

  // loop variable and current sums
  int i,k;
  T Xsum,t=0;
  
  do {
    Xsum=0;
    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
	a=(-A-I[i]-Xsum);
	b=B*(sigFunc(F,x[i])+I[i]);
	x[i]=solveRK(x[i],a,b);
	printf("%lf\n",x[i]);
      }  
    t+=DT;
  } while (t<=1); 
  do {
    Xsum=0;
    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
		printf("%lf\n",x[i]);
	a=(-A-Xsum);
	b=B*sigFunc(F,x[i]);
	x[i]=solveRK(x[i],a,b);

      }  
    t+=DT;
  } while (t<=T_MAX);
 
 for (i=0;i<N;i++)
   fprintf(output,"%d\t%lf\t%lf\n",i,x[i],I[i]);
  
/*
  // for all N neurons
  for (i=0;i<N;i++)
    {
      t=0;
      int k;
      
      // Find equilibrium values over a long period of time
      T a,b;
      do {
	Xsum=0;
	for (k=0;k<N;k++)
	  Xsum+=sigFunc(F,x[k]);

	a=(-A-I[i]-Xsum);
	b=B*(sigFunc(F,x[i])+I[i]);
	x[i]=solveRK(x[i],a,b);
	t+=DT;
      } while (t<=T_MAX);
      do {
	Xsum=0;
	for (k=0;k<N;k++)
	  Xsum+=sigFunc(F,x[k]);

	a=(-A-Xsum);
	b=B*sigFunc(F,x[i]);
	x[i]=solveRK(x[i],a,b);
	t+=DT;
      } while (t<=T_MAX);
      // write data to file
      fprintf(output,"%d\t%lf\t%lf\n",i,x[i],I[i]);
    }
  */
  fclose(output);
  return;
}

T t=0;

T A=1, B=3, F=0.25;
T I[]={0.2,0.7,0.9,0.6,0.3,0.5,0.4,0.8,0.5,0.1}, *x;//, *x=malloc(sizeof(T)*N);
  //simulate(A,B,F,I,x,'1');
  
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
	a=(-A-I[i]-Xsum);
	b=B*(sigFunc(F,x[i])+I[i]);
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
    glVertex2d(((float)i)/5-0.9, x[i]);
    glVertex2d(((float)(i+1))/5-0.9, x[i+1]);
  }
  glEnd();
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
  //glutMainLoop();
  //printf("wtf\n");
  
  while(1)
    renderFunction();

return 1;
}
