#include <stdio.h>

/* Print a Fahrenheit-Celsius conversion table */

main()
{
  int fahr;
  float celsius;
  int lower=0, upper=300, step=20;

  fahr=lower;
  while(fahr<=upper){
    celsius=5.0*(fahr-32)/9.0;
    printf("%4d%8.2f\n", fahr, celsius);
    fahr=fahr+step;
  }
}
