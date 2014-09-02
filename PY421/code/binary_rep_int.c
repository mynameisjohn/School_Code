#include <stdio.h>

/* 32 bit integers range from  -2147483648 to  2147483647 */

main()
{
  int b,c[32],i;
  printf("Enter an integer number:  ");
  scanf("%d",&b);
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
  printf("\n\n");
}
