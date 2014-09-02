/*
  CN510 Assignment 1 : plotter.c

  A simple C program that outputs the results of the analytic 
  solution to to the differential equation defined in 
  asssignment 1. The program also generates data resulting from 
  a Rotter-Deismann integration of the problem, and as a bonus 
  computes the Euler and Runge-Kutta 4 approximations as well.

  I also threw in some MPI functionality for fun, but there 
  was really no need. 

  John Joseph
  9/4/2013
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <mpi.h>

// Our time step and Max Time
#define DT 0.05
#define T_MAX 5

// The primitive data type to be used for generating data
typedef float T; 

// A method that runs several simulations based on the initial conditions
void runTests(T *x0, T t0, T I, T A, char numTest);

// The simulation methods: Analytic, Rotter-Deismann, Euler, and RK4
T solveA(T A, T D, T C, T t);
T solveRD(T x0, T A, T D);
T solveE(T x0, T I, T A);
T solveRK(T x0, T I, T A);

// An Euler-Advance method used by the Euler and RK4 approximations
T eulerAdvance(T x0, T v, T dt);

int main(int argc, char* argv[])
{
  // MPI variables, as well as variables to keep track of time
  int rank,size;  
  double t1,t2;

  // Initializing MPI and getting rank/size
  MPI_Init(&argc, &argv);
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);        /* get current process id */
  MPI_Comm_size (MPI_COMM_WORLD, &size);        /* get number of processes */
  t1 = MPI_Wtime();

  // Initial conditions of problem
  T I, A;
  
  // An array that keeps track of the final results for four simulations
  T *results = malloc(sizeof(T)*4);

  // A switch statement that puts parts of the assignment on different
  // processors
  switch(rank){
  case 0:
    {
      // Part 1
      I=5, A=1;
      runTests(results,0,I,A,'1');
      
      // Part 2
      I=0;
      runTests(results,0,I,A,'2');
    }
  default:
    {
      // Part 3
      I=5, A=2;
      runTests(results,0,I,A,'3');
      
      // Part 4
      I=0, A=1;
      runTests(results,0,I,A,'4');
    }
  }

  // A barrier so as to calculate the total runtime
  MPI_Barrier(MPI_COMM_WORLD);
  t2 = MPI_Wtime();
  printf("Elapsed time is %lf\n",t2-t1);


  MPI_Finalize();
  free(results);
  return 0;
}

void runTests(T *results, T t0, T I, T A, char numTest)
{
  // Initialize an "x" value for each simulation
  T xA=results[0], xE=results[1], xRK=results[2], xRD=results[3];
  // Variables for determining error in the simulations
  T diffE=(xA-xE), diffRK=(xA-xRK), diffRD=(xA-xRD);
  // Pre-computed values used in simulations
  T C, D;
  // Time
  T t = t0;
  // The Data file and its name
  FILE *output; char *fileName = malloc(sizeof(char)*14);
  
  // Copy the simulation number into the appropriate place
  // and open the file
  strcpy(fileName,"./data/dat_.out");
  fileName[10]=numTest;
  output=fopen(fileName,"w");
  
  // Pre compute these values
  D = I/A;
  C = xA-D;

  do  {
    // Solve analytic first to keep simulations synchronized
    xA = solveA(A, D, C, t);
        
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
    xE = solveE(xE, I, A);
    xRK = solveRK(xRK, I, A);
    xRD = solveRD(xRD, A, D);

    // Advance time
    t += DT;
  } while (t<=T_MAX+DT);

  // Put the finals results into the array, close the file, and return
  results[0]=xA; results[1]=xE; results[2]=xRK; results[3]=xRD;
  fclose(output);
  return;
}

T solveA(T A, T D, T C, T t)
{
  return C*exp(-A*t)+D;
}

T solveRD(T x0, T A, T D)
{
  return (x0-D)*exp(-A*DT)+D;
}

T solveE(T x0, T I, T A)
{
  T v = I - A*x0;
  return eulerAdvance(x0,v,DT);
}

T solveRK(T x0, T I, T A)
{
  T k1,k2,k3,k4,s;

  k1 = I - A*x0;
  k2 = I - A*eulerAdvance(x0, k1, DT/2);
  k3 = I - A*eulerAdvance(x0, k2, DT/2);
  k4 = I - A*eulerAdvance(x0 ,k3, DT);
  s = (k1+2*k2+2*k3+k4) / 6.0;

  return eulerAdvance(x0, s, DT);
}

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}
