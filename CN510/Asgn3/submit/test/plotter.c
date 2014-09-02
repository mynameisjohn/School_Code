/*
  CN510 Assignment 3 : plotter.c

  A simple C program that outputs 20 different types of
  Izhkevich Neurons. The general Izhkevich Neuron is defined by 
  a characteristic differential equation, and by changing the
  parameters of this equation we are able to produce different
  types of Izhkevich neurons. 

  John Joseph
  9/26/13
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

// Our time step and Max Time
#define DT 1
#define T_MAX 200
#define V_SPIKE 30

// The primitive data type to be used for generating data
typedef double T; 

// A method that runs a simulation based on the initial conditions
void runTests(T V0, T u0, T a, T b, T c, T d, T *I, char numTest);

// The simulation methods: Euler, and RK4
void solveE(T *x0, T I, T a, T b, T c, T d);
void solveRK(T *x0, T I, T a, T b, T c, T d);

T solveForVdot(T V0, T u0, T I);
T solveForUdot(T V0, T u0, T a, T b);

// An Euler-Advance method used by the Euler and RK4 approximations
T eulerAdvance(T x0, T v, T dt);

void constI(T *I, T val, int t0, int tMax);
void rampI(T *I, T m, T b, int t0, int tMax);

int main()
{
 // Initial conditions of problem
  T a,b,c,d,* I = malloc(sizeof(T)*(T_MAX+1));

  // Tonic Spiking
  a=0.02, b=0.2, c=-65, d=6; 
  constI(I,0,0,10);
  constI(I,14,11,T_MAX+1);
  runTests(-70,-20,a,b,c,d,I,'A');

  // Phasic Spiking
  a=0.02, b=0.25, c=-65, d=6;
  constI(I,0,0,10);
  constI(I,0.5,11,T_MAX+1);
  runTests(-70,-20,a,b,c,d,I,'B');

  // Tonic Bursting
  a=0.02, b=0.2, c=-50, d=2;
  constI(I,0,0,10);
  constI(I,15,11,T_MAX+1);
  runTests(0,0,a,b,c,d,I,'C');

  // Phasic Bursting
  a=0.02, b=0.25, c=-55, d=0.05;
  constI(I,0,0,10);
  constI(I,0.6,11,T_MAX+1);
  runTests(-70,-20,a,b,c,d,I,'D');

  // Mixed Mode
  a=0.02, b=0.2, c=-55, d=4;
  constI(I,0,0,10);
  constI(I,10,11,T_MAX+1);
  runTests(-70,-20,a,b,c,d,I,'E');

  // Spike Frequency Adaptation
  a=0.01, b=0.2, c=-65, d=8;
  constI(I,0,0,10);
  constI(I,30,11,T_MAX+1);
  runTests(-70,-20,a,b,c,d,I,'F');

  // Class 1
  a=0.02, b=-0.1, c=-55, d=6;
  constI(I,0,0,10);
  rampI(I,0.5,0,10,T_MAX+1);
  runTests(-70,0,a,b,c,d,I,'G');

  // Class 2
  a=0.2, b=0.26, c=-65, d=0;
  constI(I,0,0,10);
  rampI(I,0.05,0,10,T_MAX+1);
  runTests(-70,0,a,b,c,d,I,'H');

  // Spike Latency
  a=0.02, b=0.2, c=-65, d=6;
  constI(I,0,0,10);
  constI(I,7,10,13);
  constI(I,0,14,T_MAX+1);
  runTests(-25,-20,a,b,c,d,I,'I');

  // Sub-Threshold Oscillations
  a=0.05, b=0.26, c=-60, d=0;
  constI(I,0,0,10);
  constI(I,2,10,13);
  constI(I,0,14,T_MAX+1);
  runTests(-50,-15,a,b,c,d,I,'J');
  
  // Resonator
  a=0.1, b=0.26, c=-60, d=-1;
  constI(I,0,0,10);
  constI(I,2,10,13);
  constI(I,0,14,17);
  constI(I,2,18,21);
  constI(I,0,22,T_MAX-20);
  constI(I,2,T_MAX-35,T_MAX-33);
  constI(I,0,T_MAX-32,T_MAX-28);
  constI(I,2,T_MAX-27,T_MAX-25);
  constI(I,0,T_MAX-24,T_MAX);
  runTests(-40,0,a,b,c,d,I,'K');

   // Integrator
  a=0.02, b=-0.1, c=-55, d=6;
  constI(I,0,0,30);
  constI(I,15,30,33);
  constI(I,0,34,37);
  constI(I,15,38,41);
  constI(I,0,42,T_MAX-20);
  constI(I,15,T_MAX-35,T_MAX-33);
  constI(I,0,T_MAX-32,T_MAX-28);
  constI(I,15,T_MAX-27,T_MAX-25);
  constI(I,0,T_MAX-24,T_MAX);
  runTests(-70,-18.5,a,b,c,d,I,'L');

  // Rebound Spike
  a=0.03, b=0.25, c=-60, d=4;
  constI(I,0,0,100);
  constI(I,-80,100,103);
  constI(I,0,104,T_MAX);
  runTests(-90,20,a,b,c,d,I,'M');
  
  // Rebound Burst
  a=0.03, b=0.25, c=-52, d=0;
  constI(I,0,0,60);
  constI(I,-80,60,63);
  constI(I,0,63,T_MAX);
  runTests(-90,20,a,b,c,d,I,'N');


  // Threshold Variability
  a=0.03, b=0.25, c=-60, d=4;
  constI(I,0,0,10);
  constI(I,10,11,16);
  constI(I,0,17,T_MAX-30);
  constI(I,-10,T_MAX-29,T_MAX-24);
  constI(I,0,T_MAX-23,T_MAX-18);
  constI(I,10,T_MAX-17,T_MAX-12);
  constI(I,0,T_MAX-11,T_MAX);  
  runTests(0,-10,a,b,c,d,I,'O');
 
  // Bistability
  a=1, b=1.5, c=-60, d=0;
  constI(I,0,0,25);
  constI(I,300,25,28);
  constI(I,0,28,T_MAX-60);
  constI(I,300,T_MAX-60,T_MAX-57);
  constI(I,0,T_MAX-57,T_MAX);
  runTests(-70,-50,a,b,c,d,I,'P');
  

  // Depolarizing After-Potential
  a=1, b=0.2, c=-60, d=-21;
  constI(I,0,0,20);
  constI(I,10,20,23);
  constI(I,0,24,T_MAX+1);
  runTests(-80,10,a,b,c,d,I,'Q');
  
  // Accommodation
  a=0.02, b=1, c=-55, d=4;
  /*constI(I,-10,0,10);
  rampI(I,0.02,10,100);
  constI(I,0,100,150);
  rampI(I,1,150,154);
  constI(I,0,154,T_MAX+1);
  */
  constI(I,-35,0,10);
  rampI(I,0.1,-35,10,100);
  constI(I,-35,100,130);
  rampI(I,.7,-35,130,140);
  constI(I,-35,140,T_MAX+1);
  runTests(-80,10,a,b,c,d,I,'R');
  
  // Inhibition-Induced Spiking
  a=-0.02, b=-1, c=-60, d=8;
  constI(I,90,0,10);
  constI(I,40,10,160);
  constI(I,90,160,T_MAX+1);
  runTests(0,0,a,b,c,d,I,'S');
  
  // Inhibition-Induced Bursting
  a=-0.026, b=-1, c=-45, d=0;
  constI(I,80,0,20);
  constI(I,0,20,150);
  constI(I,80,151,T_MAX+1);
  runTests(-90,0,a,b,c,d,I,'T');
    
  return 0;
}

void runTests(T V0, T u0, T a, T b, T c, T d, T *I, char numTest)
{
  T * xE = malloc(sizeof(T)*2), * xRK =  malloc(sizeof(T)*2);
  xE[0] = V0, xE[1] = u0;
  xRK[0] = V0, xRK[1] = u0;
  T t = 0.0; //Time
  T diff = 0.0; // Difference between Euler and Runge-Kutta
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
  int i=0;
  do  {
    // Print the output to a file
    fprintf(output,
	    "%lf \t %lf \t %lf \t %lf \t %lf \t %lf \t %lf\n",
	    t,
	    xE[0],
	    xRK[0],
	    xE[1],
	    xRK[1],
	    diff,
	    I[(int)t]);
    
    
    solveE(xE, I[(int)t], a, b, c, d);
    solveRK(xRK, I[(int)t], a,b,c,d);

    //if (i%(1/DT)==0)
    //i++;
    t += DT;
  } while (t<=T_MAX);

  // Close the file and return
  fclose(output);
  return;
}

void solveE(T *x0, T I, T a, T b, T c, T d)
{
  float V, dV;
  dV = solveForVdot(x0[0],x0[1],I);
  V = eulerAdvance(x0[0],dV,DT);
  if (V<V_SPIKE)
    {
      x0[0] = V; 
      T dU = solveForUdot(x0[0],x0[1],a,b);
      x0[1] = eulerAdvance(x0[1],dU,DT);
    }
  else
    {
      x0[0]=c;
      x0[1]=x0[1]+d;
    }
  return;
}

void solveRK(T *x0, T I, T a, T b, T c, T d)
{
  T k1,k2,k3,k4,s;
  T j1,j2,j3,j4;
  float V, u;

  k1 = solveForVdot(x0[0],x0[1],I);
  j1 = solveForUdot(x0[0],x0[1],a,b);

  V = eulerAdvance(x0[0],k1,DT/2);
  u = eulerAdvance(x0[1],j1,DT/2);
  k2 = solveForVdot(V,u,I);
  j2 = solveForUdot(V,u,a,b);

  V = eulerAdvance(x0[0],k2,DT/2);
  u = eulerAdvance(x0[1],j2,DT/2);
  k3 = solveForVdot(V,u,I);
  j3 = solveForUdot(V,u,a,b);

  V = eulerAdvance(x0[0],k3,DT);
  u = eulerAdvance(x0[1],j3,DT);
  k4 = solveForVdot(V,u,I);
  j4 = solveForUdot(V,u,a,b);

  s = (k1+2*k2+2*k3+k4) / 6.0;
  V = eulerAdvance(x0[0], s, DT);

  if (V<V_SPIKE)
    {
      x0[0] = V; 
      s = (j1+2*j2+j2*j3+j4) / 6.0;
      x0[1] = eulerAdvance(x0[1], s, DT);
    }
  else
    {
      x0[0]=c;
      x0[1]=x0[1]+d;
    }
}
  
T solveForVdot(T V0, T u0, T I)
{// Equation as outlined by Izhkevich in the Assignment
  return 0.04*V0*V0+5*V0+140-u0+I;
}

T solveForUdot(T V0, T u0, T a, T b)
{// Equation as outlined by Izhkevich in the Assignment
  return a*(b*V0-u0);
}

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

void constI(T *I, T val, int t0, int tMax)
{
  int i;
  for (i=t0;i<tMax;i++)
    I[i]=val;
}

void rampI(T *I, T m, T b, int t0, int tMax)
{
  // Why no for loop? ha ha
  int i=t0;
  do {
    I[i]=b+m*(T)(i-t0);
    i++;
  } while (i<tMax);
}
