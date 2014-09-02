#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define DT 0.05
#define T_MAX 1000

typedef double T;

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

T solveRK (T x0, T I, T A, T B, T sum)
{
  T k1,k2,k3,k4,s;
  T q = A+sum;
  k1 = -q*x0+B*I;//I - A*result;
  k2 = -q*eulerAdvance(x0,k1,DT/2)+B*I;//I - A*eulerAdvance(x0,k1,DT/2);
  k3 = -q*eulerAdvance(x0,k2,DT/2)+B*I;//I - A*eulerAdvance(result,k2,DT/2);
  k4 = -q*eulerAdvance(x0,k3,DT)+B*I;
  s = (k1+2*k2+2*k3+k4);
  return eulerAdvance(x0,s,DT/6.0);
}

int main()
{
  T A=0.1, B=1, sum1=0, sum2=0, SIG1=0, SIG2=0;
  T I2[] = {10,9,8,7,6,5,4,3,2,1};
  T I1[] = {1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1};
  T *x1 = malloc(sizeof(T)*10);
  T *x2 = malloc(sizeof(T)*10);

  int i;
  FILE *output = fopen("data/64/testB.out","w");

  for (i=0;i<10;i++)
    sum1+=I1[i];
  for (i=0;i<10;i++)
    sum2+=I2[i];

  for (i=0;i<10;i++)
    {
      T t=0; //x1=0,x2=0,X1,X2;
      do {
	x1[i]=solveRK(x1[i],I1[i],A,B,sum1);
	x2[i]=solveRK(x2[i],I2[i],A,B,sum2);
	t+=DT;
      } while (t<T_MAX);
      SIG1+=fabs(x1[i]);
      SIG2+=fabs(x2[i]);
      //fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\n",i,x1,x2,SIG1,SIG2);
    }
  for (i=0;i<10;i++)
    fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\n",i,x1[i],x2[i],x1[i]/SIG1,x2[i]/SIG2);
      
  fclose(output);
  return 1;
}
