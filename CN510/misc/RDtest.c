#include "CNTools.h"
#include "stdio.h"


int main(){

  

  T x=1,A=50.0/1000.0;
  T t=0,tMax=12000;
  while (t<=tMax){
    x=solveRD(x,A,0);
    if (x>0.1) printf("%lf\n",x);
    t+=DT;
  }

  return 1;
}
