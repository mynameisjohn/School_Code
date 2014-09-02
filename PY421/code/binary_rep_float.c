#include <stdio.h>

main()
{
  float a;
  int *ap;
  int b,c[32],i;
  printf("Enter a floating point number:  ");
  scanf("%f",&a);
  ap=(int *)&a;
  b=*ap;
  printf("\nThe number in hexadecimal representation:\n %x\n",b);
  for(i=0;i<32;i++){
    c[i]=b&1;
    b=b>>1;
  }
  printf("\nThe number in binary:\n");
  for(i=31;i>=0;i--){
    if(!((i+1)%4)) printf(" ");
    printf("%1d",c[i]);
  }
  printf("\n");
  printf("\nThe binary grouped as sign, exponent, and mantissa:\n");
  for(i=31;i>=0;i--){
    if(i==31||i==30||i==22) printf(" ");
    printf("%1d",c[i]);
  }
  printf("\n\n");
}
