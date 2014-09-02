#include <stdio.h>
#include <math.h>

/* Approximate the first and second derivative */

main()
{

  double x, dx, d1ex, d1f, d1b, d1c, d2ex, d2c;

  printf("Enter the argument of sin(x): ");
  scanf("%lf",&x);
  printf("Enter the interval:  ");
  scanf("%lf",&dx);
  printf("\n");
  
  d1ex=cos(x);
  d2ex=-sin(x);

  d1f=(sin(x+dx)-sin(x))/dx;
  printf("Forward diff. approx.  %12.9f, exact %12.9f, error %12.9f\n",d1f,d1ex,d1f-d1ex);
  d1b=(sin(x)-sin(x-dx))/dx;
  printf("Backward diff. approx. %12.9f, exact %12.9f, error %12.9f\n",d1b,d1ex,d1b-d1ex);
  d1c=(sin(x+dx)-sin(x-dx))/(2*dx);
  printf("Central diff. approx.  %12.9f, exact %12.9f, error %12.9f\n",d1c,d1ex,d1c-d1ex);

  printf("\n");
  d2c=(sin(x+dx)+sin(x-dx)-2*sin(x))/(dx*dx);
  printf("Second der. c. approx. %12.9f, exact %12.9f, error %12.9f\n",d2c,d2ex,d2c-d2ex);

}
