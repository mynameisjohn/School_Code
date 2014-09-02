#include<stdio.h>
#include<math.h>

main(){

  int n;
  double x;
  double ee;
  double e=1, y=1;
  int k;

  printf("Enter x and the maximum degree in the expansion: ");
  scanf("%lf%d",&x,&n);

  ee=exp(x);
  printf("Exact value of exp(x): %14.9f\n",ee);

  y=1;
  e=1;
  for(k=1;k<=n;k++){
    y*=x;
    y/=k;
    e+=y;
  }

  printf("Power series value of exp(x): %14.9f\n",e);

}
