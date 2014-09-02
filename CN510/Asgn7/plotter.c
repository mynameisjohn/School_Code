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

#define DT 0.05
#define T_MAX 10
#define N 3

//The data Type used for simulation
typedef double T;

T max (T a, T b){
  return (a<b) ? b : a;}

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

void simulate(T A0,T I0,T x0,T A,T B,T C,T D,T Gamma,T tau,T *I, T *x,T *w,char numTest)
{
  // naming the output file
  FILE *output, *tOutput, *pOutput; char *fileName = malloc(sizeof(char)*14);
  if (sizeof(T)==4)
    strcpy(fileName,"./data/32/dat_.out");
  else
    strcpy(fileName,"./data/64/dat_.out");
  fileName[13]=numTest;
  output=fopen(fileName,"w");
  
  T *X=malloc(sizeof(T)*N),*W=malloc(sizeof(T)*N),*Th=malloc(sizeof(T)*N);
  T Xsum=0,Wsum=0,Isum=0;
  T t=0;
  int i;

  for (i=0;i<N;i++)
    {
      X[i]=0;
      W[i]=0;
      Th[i]=0;
    }
  for (i=0;i<N;i++)
    {
      Xsum+=x[i];
      Wsum+=w[i];
      Isum+=I[i];
    }
  for (i=0;i<N;i++)
    {
      X[i]=x[i]/Xsum;
      W[i]=w[i]/Wsum;
      Th[i]=I[i]/Isum;
    }
  fprintf(output,"%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",
	  t,x[0],x[1],x[2],w[0],w[1],w[2],I[0],I[1],I[2],
	  X[0],X[1],X[2],W[0],W[1],W[2],Th[0],Th[1],Th[2]);
  

  T x0Tmp,xTmp,wTmp;
  T xCoef,wCoef;
  while (t<=T_MAX)
    {
      x0Tmp=x0;
      x0=solveRK(x0,-A0,((t<=2) ? 0 : I0));
      //printf("%lf\n",((t<=2) ? 0 : I0));
      xCoef=B*max((I0/A0)*(1-exp(-A0*(t-tau))-Gamma),0);
      wCoef=D*max((I0/A0)*(1-exp(-A0*(t-tau))-Gamma),0);

      for (i=0;i<N;i++)
	{
	  xTmp=x[i],wTmp=w[i];
	  x[i]=solveRK(x[i],-A,xCoef*w[i]+I[i]);
	  w[i]=solveRK(w[i],-C,wCoef*xTmp);
	}
      /*
      for (i=0;i<N;i++)
	{
	  X[i]=0;
	  W[i]=0;
	  Th[i]=0;
	  }*/
      Xsum=0,Wsum=0,Isum=0;
      for (i=0;i<N;i++)
	{
	  Xsum+=x[i];
	  Wsum+=w[i];
	  Isum+=I[i];
	}
      for (i=0;i<N;i++)
	{
	  X[i]=x[i]/Xsum;
	  W[i]=w[i]/Wsum;
	  Th[i]=I[i]/Isum;
	}
      
      t+=DT;
      //printf("%lf\n",t);
      //printf("%lf\n",X[0]);
      fprintf(output,"%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",
	      t,x[0],x[1],x[2],w[0],w[1],w[2],I[0],I[1],I[2],
	      X[0],X[1],X[2],W[0],W[1],W[2],Th[0],Th[1],Th[2]);
    }
  //printf("%lf\n",max(-1,0));
  fclose(output);
  return;
}

int main()
{
  // Initializes the clock, as well A, B, and an array for I
  //clock_t start = clock(), diff;
  
  T A0=1, I0=1, x0=0, A=5, B=1, C=1, D=1, Gamma=0.2, tau=0.05;
  T I1[]={0.1,0.7,0.2};
  T x1[]={0.6,0.1,0.3};
  T w1[]={0.7,0.2,0.1};
  simulate(A0,I0,x0,A,B,C,D,Gamma,tau,I1,x1,w1,'1');
  T I2[]={0.1,0.7,0.2};
  T x2[]={0.6,0.1,0.3};
  T w2[]={0.7,0.2,0.1};
  x0=0;
  A=0.5;
  simulate(A0,I0,x0,A,B,C,D,Gamma,tau,I2,x2,w2,'2');
  return 1;
}