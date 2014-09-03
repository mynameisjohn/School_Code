#include <math.h>
#ifndef CONSTANTS
#define CONSTANTS
#define DT 1
#define N 100
#define NSPIKE 60
#define VTHRESH 400
#define VRESET -5
#endif
//The data Type used for simulation
typedef float T;

//Neuron Struct
struct Neuron{
  // array of spike times
  int spikeTrain[NSPIKE];
  int index,s;
  T w[20],x;
};

//Maximum of two values
T max (T a, T b)
  {return (a<b) ? b : a;}

//Figure it out
T min (T a, T b)
   {return (a>b) ? b : a;}

//Ensures m <= v <= M
T clamp (T v, T m, T M)
   {return min(max(v,m),M);}

// Rotter-Diessmann integration
T solveRD (T x0, T a, T b)
{
  return x0*exp(-a*DT)+b;
}

// Euler Advance
T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

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
