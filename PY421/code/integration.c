/* This program illustrates numerical integration formulae
   applying them to the calculation of the integral of sin(x)
   between 0.5 and 1.5.

   Claudio Rebbi, January 2009 */

#include<stdio.h>
#include<math.h>

main()
{
  int n;
  double x0=0.5,xn=1.5;
  double a,x,dx;
  int k;
  double ex;
  double r=0.774596669241483;
  
  ex=cos(x0)-cos(xn);

  printf("Enter the number of subintervals: ");
  scanf("%d",&n);
  printf("\n");
  
  dx=(xn-x0)/n;

  /* trapezoidal formula */

  a=sin(x0)/2;
  for(k=1;k<n;k++){
    x=x0+k*dx;
    a+=sin(x);
  }
  a+=sin(xn)/2;

  a*=dx;

  printf("Trapezoidal formula: %12.9f, err.: %12.9f, err./dx**2: %12.9f\n",a,ex-a,(ex-a)/pow(dx,2));

  /* midpoint-based trapeziodal formula */

  a=0;
  for(k=0;k<n;k++){
    x=x0+(k+0.5)*dx;
    a+=sin(x);
  }

  a*=dx;

  printf("Mid-point trapez.:   %12.9f, err.: %12.9f, err./dx**2: %12.9f\n",a,ex-a,(ex-a)/pow(dx,2));

  /* Simpson's formula */

  /*
  printf("Simpson's formula:   %12.9f, err.: %12.9f, err./dx**4: %12.9f\n",a,ex-a,(ex-a)/pow(dx,4));
  */

  /* Gaussian quadrature formula */

  a=0;
  for(k=0;k<n;k++){
    x=x0+(k+0.5)*dx;
    a+=(4*sin(x))/9;
    x=x-0.5*r*dx;
    a+=(5*sin(x))/18;
    x=x+r*dx;
    a+=(5*sin(x))/18;
  }

  a*=dx;

  printf("Gaussian quadrature: %12.9f, err.: %12.9f, err./dx**6: %12.9f\n",a,ex-a,(ex-a)/pow(dx,6));

}

