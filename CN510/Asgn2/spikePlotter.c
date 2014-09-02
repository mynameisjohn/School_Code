/*
  CN510 Assignment 2 : spikePlotter.c

  A simple C program that plots the solution to the proposed equation for the
  Leaky Integrate and Fire model of the neuron. The algorithm is almost
  identical to that of the Leaky Integrator, save for the condition that we 
  generate a "spike" value if our output crosses some threshold. As with the
  last assignment, this data will be output both numerically and analytically. 

  John Joseph
  9/21/2013
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

// Our time step and Max Time
#define DT 0.05
#define T_MAX 10
#define V_SPIKE 60
#define V_RESET -1

// The primitive data type to be used for generating data
typedef double T; 

// A method that runs several simulations based on the initial conditions
void runTests(T *x0, T t0, T A, T vThresh, char numTest);

// The simulation methods: Analytic, Rotter-Deismann, Euler, and RK4
T solveA(T x0, T A, T I, T t, T vThresh);
T solveRD(T x0, T A, T I, T vThresh);
T solveE(T x0, T I, T A, T vThresh);
T solveRK(T x0, T I, T A, T vThresh);

// An Euler-Advance method used by the Euler and RK4 approximations
T eulerAdvance(T x0, T v, T dt);

int main()
{
 // Initial conditions of problem, now including our threshold voltage
  T I, A, vThresh;
  
  // An array that keeps track of the final results for four simulations
  T *results = malloc(sizeof(T)*4);

  // Part 1
  A=1,vThresh=1;
  runTests(results,0,A,vThresh,'1');
  
  // Part 2
  vThresh=2;
  runTests(results,0,A,vThresh,'2');
  
  // Free up the results array and return
  free(results);
  return 0;
}

void runTests(T *results, T t0, T A, T vThresh, char numTest)
{
  // Initialize an "x" value for each simulation
  T xA=results[0], xE=results[1], xRK=results[2], xRD=results[3];
  // Variables for determining error in the simulations
  T diffE=(xA-xE), diffRK=(xA-xRK), diffRD=(xA-xRD);
  // Pre-computed values used in simulations
  T I = 0;
  // Time
  T t = t0;
  // The data file and its name
  FILE *output; char *fileName = malloc(sizeof(char)*14);
  // Copy the simulation number into the appropriate place
  // and open the file
  if (sizeof(T)==4)
    strcpy(fileName,"./data/32/dat_.out");
  else
    strcpy(fileName,"./data/64/dat_.out");
  fileName[13]=numTest;
  output=fopen(fileName,"w");
  
  do  {
    xA = solveA(xA, A, I, t, vThresh);
        
    diffE = fabs(xA-xE);
    diffRK = fabs(xA-xRK);
    diffRD = fabs(xA-xRD);

    // Print the output to a file
    fprintf(output,
	    "%lf \t %lf \t %lf \t %lf \t %lf \t %lf\t %lf \t %lf\n",
	    t,
	    xA,
	    xE,
	    xRD,
	    xRK,
	    diffE,
	    diffRK,
	    diffRD);
    
    // Run the three numerical simulations
    xE = solveE(xE, I, A, vThresh);
    xRK = solveRK(xRK, I, A, vThresh);
    xRD = solveRD(xRD, A, I, vThresh);

    if (t>=6)
      I=0;
    else if (t>=1)
      I=3;

    // Advance time
    t += DT;
  } while (t<=T_MAX+DT);

  // Put the final results into the array, close the file, and return
  results[0]=xA; results[1]=xE; results[2]=xRK; results[3]=xRD;
  fclose(output);
  return;
}


// This code contains spiking functionality, but it's useless
T solveA(T x0, T A, T I, T t, T vThresh)
{
   if (x0==V_SPIKE)
    return -1.0;
  T ret = (x0-(I/A))*exp(-A*t)+(I/A);
  if (ret>vThresh)
    return V_SPIKE;
  return ret;
}

// Simple conditionals to handle spike/reset
T solveRD(T x0, T A, T I, T vThresh)
{
  if (x0==V_SPIKE)
    return V_RESET;
  T ret = (x0-(I/A))*exp(-A*DT)+(I/A);
  if (ret>vThresh)
    return V_SPIKE;
  return ret;
}

// Simple conditionals to handle spike/reset
T solveE(T x0, T I, T A, T vThresh)
{
  if (x0==V_SPIKE)
    return V_RESET;
  T v = I - A*x0;
  T ret = eulerAdvance(x0,v,DT);
   if (ret>vThresh)
    return V_SPIKE;
  return ret;
}

// Simple conditionals to handle spike/reset
T solveRK(T x0, T I, T A, T vThresh)
{
  if (x0==V_SPIKE)
    return V_RESET;
  T k1,k2,k3,k4,s;

  k1 = I - A*x0;
  k2 = I - A*eulerAdvance(x0, k1, DT/2);
  k3 = I - A*eulerAdvance(x0, k2, DT/2);
  k4 = I - A*eulerAdvance(x0 ,k3, DT);
  s = (k1+2*k2+2*k3+k4) / 6.0;

  T ret = eulerAdvance(x0, s, DT);
  
  if (ret>vThresh)
    return V_SPIKE;
  return ret;
}

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}
