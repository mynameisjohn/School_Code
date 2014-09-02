/*
  CN510 Assignment 1 : plotter.c

  A simple C program that outputs the results of the analytic 
  solution to to the differential equation defined in 
  asssignment 1. 

  John Joseph
  9/4/2013
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#define DT 0.05
#define T_MAX 5

void runTests(float *x0, float t0, float I, float A, char numTest);

float solveA(float A, float D, float C, float t);
float solveE(float x0, float I, float A);
float solveRK(float x0, float I, float A);

float solveRD(float x0, float I, float A);

float eulerAdvance(float x0, float v, float dt);

void main()
{
  float I, A;
  float *results = malloc(sizeof(float)*4);
 
  I=5, A=1;
  runTests(results,0,I,A,'1');

  I=0;
  runTests(results,0,I,A,'2');

  I=5, A=2;
  runTests(results,0,I,A,'3');

  I=0, A=1;
  runTests(results,0,I,A,'4');

  free(results);
  return;
}

void runTests(float *results, float t0, float I, float A, char numTest)
{
  float xA=results[0], xE=results[1], xRK=results[2], xRD=results[3];
  float diffE=(xA-xE), diffRK=(xA-xRK), diffRD=(xA-xRD);
  float C, D;
  float t = t0;
  FILE *output;
  char *fileName = malloc(sizeof(char)*14);

  strcpy(fileName,"./data/dat_.out");
  fileName[10]=numTest;
  printf("%s\n",fileName);
  output=fopen(fileName,"w");
  
  D = I/A;
  C = xA-D;

  do  {
    fprintf(output,"%lf \t %lf \t %lf \t %lf \t %lf \t %lf\t %lf \t %lf\n",
	    t,xA,xRD,xE,xRK,diffE,diffRK,diffRD);
     
    xA = solveA(A, D, C, t);
    xE = solveE(xE, I, A);
    xRK = solveRK(xRK, I, A);
    xRD = solveRD(xRD,I,A);   

    diffE = fabs(xA-xE);
    diffRK = fabs(xA-xRK);
    diffRD = fabs(xA-xRD);

    t += DT;
  } while (t<=T_MAX+DT);
  results[0]=xA; results[1]=xE; results[2]=xRK; results[3]=xRD;
  fclose(output);
  return;
}

float solveA(float A, float D, float C, float t)
{
  return C*exp(-A*t)+D;
}


float solveRD(float x0, float I, float A)
{
  return (x0-(I/A))*exp(-A*DT)+(I/A);
}


float solveE(float x0, float I, float A)
{
  float v = I - A*x0;
  return eulerAdvance(x0,v,DT);
}

float solveRK(float x0, float I, float A)
{
  float k1,k2,k3,k4,s;

  k1 = I - A*x0;
  k2 = I - A*eulerAdvance(x0, k1, DT/2);
  k3 = I - A*eulerAdvance(x0, k2, DT/2);
  k4 = I - A*eulerAdvance(x0 ,k3, DT);
  s = (k1+2*k2+2*k3+k4) / 6.0;

  return eulerAdvance(x0, s, DT);
}

float eulerAdvance(float x0, float v, float dt)
{return x0 + dt * v;}
