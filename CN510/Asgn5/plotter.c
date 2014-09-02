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

void simulate(T A, T B, T F, T *I, T *x, char numTest)
{
  // naming the output file
  FILE *output, *tOutput, *pOutput; char *fileName = malloc(sizeof(char)*14);
  if (sizeof(T)==4)
    strcpy(fileName,"./data/32/dat_.out");
  else
    strcpy(fileName,"./data/64/dat_.out");
  fileName[13]=numTest;
  output=fopen(fileName,"w");

  fileName[13]='T';
  tOutput = fopen(fileName,"w");

  fileName[13]='P';
  pOutput = fopen(fileName,"w");

  // loop variable and current sums
  int i,k,count=0,printRate=2;
  T Xsum,t=0,X=0;

  //Initialize sum variable
  for (i=0;i<N;i++)
    X+=x[i];

  //Print to our files
  fprintf(tOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]);  
  fprintf(pOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]);
 
  do {
    Xsum=0;
    // Fill up the sum term
    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
	a=(-A-I[i]-Xsum);
	b=B*(sigFunc(F,x[i])+I[i]);
	x[i]=solveRK(x[i],a,b);
      }  
    count++;
    t+=DT;
    if (count>printRate)
      {
	 for (i=0;i<N;i++)
	   X+=x[i];
	 count=0;

	 fprintf(tOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]);
	 fprintf(pOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0]/X,x[1]/X,x[2]/X,x[3]/X,x[4]/X,x[5]/X,x[6]/X,x[7]/X,x[8]/X,x[9]/X);
	 X=0;
      }
  } while (t<=1); 
  do {
    Xsum=0;
    for (k=0;k<N;k++)
      Xsum+=sigFunc(F,x[k]);
    T a,b;
    for (i=0;i<N;i++)
      {
\	a=(-A-Xsum);
	b=B*sigFunc(F,x[i]);
	x[i]=solveRK(x[i],a,b);

      }  
    t+=DT;
    count++;
    if (count>printRate)
      {
	 for (i=0;i<N;i++)
	   X+=x[i];
	count=0;

	fprintf(tOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]);	
  fprintf(pOutput,"%lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\t %lf\n",t,x[0]/X,x[1]/X,x[2]/X,x[3]/X,x[4]/X,x[5]/X,x[6]/X,x[7]/X,x[8]/X,x[9]/X);
  X=0;
      }
  } while (t<=T_MAX);

  for (i=0;i<N;i++)
    X+=x[i];

 for (i=0;i<N;i++)
   fprintf(output,"%d\t%lf\t%lf\t%lf\n",i,x[i],x[i]/X,I[i]);
  
  fclose(output);
  fclose(tOutput);
  fclose(pOutput);
  return;
}

int main()
{
  // Initializes the clock, as well A, B, and an array for I
  clock_t start = clock(), diff;
  T A=1, B=3, F=0.25;
  T I[]={0.2,0.7,0.9,0.6,0.3,0.5,0.4,0.8,0.5,0.1};
  T x[]={0.7,0.6,0.8,0.9,0.5,0.3,0.5,0.7,0.8,0.4};
  simulate(A,B,F,I,x,'1');
  return 1;
}
