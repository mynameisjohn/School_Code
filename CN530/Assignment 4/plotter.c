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

#include "GL/freeglut.h"
#include "GL/gl.h"

#define DT 0.0005
#define T_MAX 10
#define N 100

//The data Type used ifor simulation
typedef float T;

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

T sigFunc(T F, T w)
{
  //Linear
  return w;
  //Faster than linear
  return w*w;
  //Slower than linear
  return w/(F+w);
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

T t=0.0;
unsigned char init=0;
char buffer[10];

void renderFunction()
{
   T A=1,B=1.0,C=2.0,D=1.0,E=0.5;
   T mu=0.05,nu=0.005;
   T m=0.5, SOA=1.5;
   static T * I, *x, *a, *b;
   static FILE * s1, * s2, * travel1, * travel2, * travel3;
   static FILE * s1D1, * s1D2, * s1D3, * s1D4;
   T alpha=0.0,beta=0.0;
   int i,j;
   int k=55,l=65;

   if (!init){
      I = calloc(N,sizeof(T));
      x = calloc(N,sizeof(T));
      a = calloc(N,sizeof(T));
      b = calloc(N,sizeof(T));
      s1 = fopen("stimulus1.txt","w");
      s1D1 = fopen("stimulus1D1.txt","w");
      s1D2 = fopen("stimulus1D2.txt","w");
      s1D3 = fopen("stimulus1D3.txt","w");
      s1D4 = fopen("stimulus1D4.txt","w");
      s2 = fopen("stimulus2.txt","w");
      travel1 = fopen("travel1.txt","w");
      travel2 = fopen("travel2.txt","w");
      travel3 = fopen("travel3.txt","w");
//      for (i=0;i<N;i++)
//	 I[i] = (i<N/2) ? 0.0 : 10.0; 
      init = 1;
   }
  
   if (t<T_MAX){
      for (i=0;i<N;i++){
	 //Take sums
	 for (j=0;j<N;j++){
	    T r = (T)((j-i)%N);
	    beta  += C*I[j]*exp(-mu*r*r);
	    alpha += E*I[j]*exp(-nu*r*r);
	 }

	 a[i]=-(A+beta+alpha);
	 b[i]=B*beta-D*alpha;
	 alpha=0.0;
	 beta=0.0;
      }    
      if (t>0.0f && t<0.0f+m)
	 //for (i=0;i<N;i++)
         I[k] = 10.0f;// I[50] = 0.0f;}//(i>N/2) ? 0.0 : 10.0;
      else
	 I[k]=0.0f;

      if (t>SOA && t<SOA+m)
         //for (i=0;i<N;i++)
         I[l] = 10.0f;// I[50] = 0.0f;}//(i>N/2) ? 0.0 : 10.0;
      else
         I[l]=0.0f;

      //print first stimuli
      if (fabs(t-0.5f)<0.0001){
	 printf("printing first stimuli\n");
	 for (i=0;i<N;i++)
	    fprintf(s1,"%d\t%lf\n",i,x[i]);
	 fclose(s1);
      }

      if (fabs(t-0.7f)<0.0001){
         printf("printing first stimuli\n");
         for (i=0;i<N;i++)
            fprintf(s1D1,"%d\t%lf\n",i,x[i]);
         fclose(s1D1);
      }

      if (fabs(t-0.9f)<0.0001){
         printf("printing first stimuli\n");
         for (i=0;i<N;i++)
            fprintf(s1D2,"%d\t%lf\n",i,x[i]);
         fclose(s1D2);
      }

      if (fabs(t-1.1f)<0.0001){
         printf("printing first stimuli\n");
         for (i=0;i<N;i++)
            fprintf(s1D3,"%d\t%lf\n",i,x[i]);
         fclose(s1D3);
      }

      if (fabs(t-1.3f)<0.0001){
         printf("printing first stimuli\n");
         for (i=0;i<N;i++)
            fprintf(s1D4,"%d\t%lf\n",i,x[i]);
         fclose(s1D4);
      }
      
      if (fabs(t-1.52f)<0.0001){
	 printf("printing second stimuli\n");
         for (i=0;i<N;i++)
            fprintf(travel1,"%d\t%lf\n",i,x[i]);
	 fclose(travel1);
      }
   
      if (fabs(t-1.55f)<0.0001){
         printf("printing second stimuli\n");
         for (i=0;i<N;i++)
            fprintf(travel2,"%d\t%lf\n",i,x[i]);
         fclose(travel2);
      }

      if (fabs(t-1.6f)<0.0001){
         printf("printing second stimuli\n");
         for (i=0;i<N;i++)
            fprintf(travel3,"%d\t%lf\n",i,x[i]);
         fclose(travel3);
      }

      if (fabs(t-2.0f)<0.0001){
	 printf("printing third stimuli\n");
         for (i=0;i<N;i++)
            fprintf(s2,"%d\t%lf\n",i,x[i]);
	 fclose(s2);
      }


      for (i=0;i<N;i++)
	 x[i]=solveRK(x[i],a[i],b[i]);
   
   t += DT;
   glClearColor(0.0, 0.0, 0.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT);
   glColor3f(1.0, 1.0, 1.0);
   glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   glBegin(GL_LINES);
   for (i=0;i<N-1;i++){
      glVertex2f((2.0f*((T)i/N)-1.0f), x[i]);
      glVertex2f((2.0f*((T)(i+1)/N)-1.0f), x[i+1]);
   }
   glEnd();
   }
   else if (init==1){
      free(I);
      free(x);
      free(a);
      free(b);
      init=2;
   }
   glRasterPos2f(0.0f,-0.5f);
   glColor4f(0.0f,0.0f,1.0,1.0f);
   sprintf(buffer,"%6.2f",t);
   glutBitmapString(GLUT_BITMAP_HELVETICA_18, buffer);
   glFlush();
  
   /*
   free(I);
   free(x);
   free(a);
   free(b);
   */
}

int main(int argc, char** argv)
{
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

