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
void checkErrors();

float solveA(float x0, float I, float A, float t, FILE *output);
float solveE(float x0, float I, float A, float t, FILE *output);
float solveRK(float x0, float I, float A, float y, FILE *output);

float eulerAdvance(float x0, float v, float dt);

void main()
{
  FILE *output;
  float I, A, C, D, xA, xE, xRK;
  float *results = malloc(sizeof(float)*3);

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

void runTests(float *x0, float t0, float I, float A, char numTest)
{
  float xA,xE,xRK;
  FILE *output;
  char *fileName = malloc(sizeof(char)*14);
  strcpy(fileName,"./data/");

  strcpy(fileName+7,"a_.out");
  fileName[8]=numTest;
  output = fopen(fileName,"w");
  x0[0]=solveA(x0[0],I,A,t0,output);
  fclose(output);

  strcpy(fileName+7,"e_.out");
  fileName[8]=numTest;
  output = fopen(fileName,"w");
  x0[1]=solveE(x0[1],I,A,t0,output);
  fclose(output);

  strcpy(fileName+7,"rk_.out");
  fileName[9]=numTest;
  output = fopen(fileName,"w");
  x0[2]=solveRK(x0[2],I,A,t0,output);
  fclose(output);
  
  free(fileName);
}

float solveA(float x0, float I, float A, float t, FILE *output)
{
  float D = I/A;
  float C = x0 - D;
  float result;
  do  
    {
      result = C*exp(-A*t)+D;
      fprintf(output,"%lf \t %lf\n",t,result);
      t += DT;
    } while (t<=T_MAX+DT);
  return result;
}

float solveE(float x0, float I, float A, float t, FILE *output)
{
  float result = x0;
  do {
    fprintf(output,"%lf \t %lf\n",t,result);
    float v = I - A*result;
    result=eulerAdvance(result,v,DT);
    t+=DT;
  } while (t<=T_MAX+DT);
  return result;
}

float solveRK(float x0, float I, float A, float t, FILE *output)
{
  float k1,k2,k3,k4,s;
  float result = x0;
  do {
    fprintf(output,"%lf \t %lf\n",t,result);
    k1 = I - A*result;
    k2 = I - A*eulerAdvance(result,k1,DT/2);
    k3 = I - A*eulerAdvance(result,k2,DT/2);
    k4 = I - A*eulerAdvance(result,k3,DT);
    s = (k1+2*k2+2*k3+k4);
    result = eulerAdvance(result,s,DT/6.0);
    t+=DT;
  } while (t<=T_MAX+DT);
  return result;
}

float eulerAdvance(float x0, float v, float dt)
{
  return x0 + dt * v;
}

