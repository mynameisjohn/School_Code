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
#define T_MAX 10000
#define N 100

//The data Type used for simulation
typedef float T;

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
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

void simulate(T A, T B, T C, T F, T G, T *I, char numTest)
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
  int i;
  T Esum, Isum;
  
  // Our sum-radius (nothing outside of it should be included)
  int rE = sqrt(-F*F*log(0.01));
  int rI = sqrt(-G*G*log(0.02));
  
  // for all N neurons
  for (i=0;i<N;i++)
    {  
      Esum = 0, Isum = 0;
      int k;
      // Calculate the excitatory and inhibitory sums
      for (k=-rE;k<=rE;k++)
	{ 
	  int r = (i+k+N)%N;
	  T e = -(k/F)*(k/F);
	  Esum += I[r]*exp(e);
	}
      // Note the wrap around (modulus %) condition
      for (k=-rI;k<=rI;k++)
	{ 
	  int r = (i+k+N)%N;
	  T e = -(k/G)*(k/G);
	  Isum += 0.5*I[r]*exp(e);
	}
      // Find equilibrium values over a long period of time
      T x=0,t=0;
      // I can't explain the /100, other than numerical error, but
      // EQ values should still be the same
      T a = (-A-Esum-Isum)/100;
      T b = (B*Esum-C*Isum)/100;
      do {
	x=solveRK(x,a,b);
	t+=DT;
      } while (t<=T_MAX);
      // write data to file
      fprintf(output,"%d\t%lf\n",i+1,x);
    }

  fclose(output);
  return;
}

int main()
{
  // Initializes the clock, as well A, B, and an array for I
  clock_t start = clock(), diff;
  T A=1, B=10, C=1.5, *I=malloc(sizeof(T)*N);
  int i;
  
  // Now we run our 8 simulations; two F and G values
  // with four different current configurations
  T F=2, G=4;
  for (i=0;i<25;i++)
    I[i]=10;
  for (i=25;i<75;i++)
    I[i]=80;
  for (i=75;i<100;i++)
    I[i]=10;
  simulate(A,B,C,F,G,I,'1');

// Calculate the runtime and return
  diff = clock() - start;
  int msec = diff * 1000 / CLOCKS_PER_SEC;
  printf("The simulation took %d milliseconds. \n", msec);
 
  F=4, G=8;
  simulate(A,B,C,F,G,I,'2');

  F=2, G=4;
  for (i=0;i<25;i++)
    I[i]=1;
  for (i=25;i<75;i++)
    I[i]=8;
  for (i=75;i<100;i++)
    I[i]=1;
  simulate(A,B,C,F,G,I,'3');

  F=4, G=8;
  simulate(A,B,C,F,G,I,'4');

  F=2, G=4;
  for (i=0;i<45;i++)
    I[i]=10;
  for (i=45;i<55;i++)
    I[i]=80;
  for (i=55;i<100;i++)
    I[i]=10;
  simulate(A,B,C,F,G,I,'5');

  F=4, G=8;
  simulate(A,B,C,F,G,I,'6');

  F=2, G=4;
  for (i=0;i<10;i++)
    I[i]=1;
  for (i=10;i<90;i++)
    I[i]=1+i-9;
  for (i=90;i<100;i++)
    I[i]=82;
  simulate(A,B,C,F,G,I,'7');

  F=4, G=8;
  simulate(A,B,C,F,G,I,'8');

  
  return 1;
}