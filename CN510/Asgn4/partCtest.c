#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define DT 0.05
#define T_MAX 100

typedef double T;

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}

T solveRK (T x0, T I, T A, T B, T sum)
{
  T k1,k2,k3,k4,s;
  T q = (1+35+35)/10;
  T u = (10*35-1.5*35)/10;
  k1 = -q*x0+u;//I - A*result;
  k2 = -q*eulerAdvance(x0,k1,DT/2)+u;//I - A*eulerAdvance(x0,k1,DT/2);
  k3 = -q*eulerAdvance(x0,k2,DT/2)+u;//I - A*eulerAdvance(result,k2,DT/2);
  k4 = -q*eulerAdvance(x0,k3,DT)+u;
  s = (k1+2*k2+2*k3+k4);
  return eulerAdvance(x0,s,DT/6.0);
}

int main()
{
  T A=0.1, B=1, sum=0, I[] = {1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1};
  int i;
  FILE *output = fopen("data/64/testB.out","w");

  for (i=0;i<10;i++)
    sum+=I[i];
  printf("%lf\n",sum);

  for (i=0;i<10;i++)
    {
      T t=0, x=0;
      do {
	x=solveRK(x,I[i],A,B,sum);
	//fprintf(output,"%lf\t%lf\n",t,x);
	t+=DT;
      } while (t<T_MAX);
      fprintf(output,"%d\t%lf\n",i,x);
      printf("%lf\n",x);
    }

  fclose(output);
  return 1;
}
