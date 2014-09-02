#include <stdio.h>

#define MAXDEGREE 12

main()
{
  int coeff[MAXDEGREE+1][MAXDEGREE+3];
  int i,j;
  
  i=0;
  coeff[i][0]=0; coeff[i][1]=1; coeff[i][2]=0; 
  for(j=1;j<=i+1;j++){
    printf("%5d",coeff[i][j]);
  }
  printf("\n");
  
  for(i=1;i<=MAXDEGREE;i++){
    coeff[i][0]=0;coeff[i][i+2]=0;
    for(j=1;j<=i+1;j++){
      coeff[i][j]=coeff[i-1][j-1]+coeff[i-1][j];
    }
    for(j=1;j<=i+1;j++){
      printf("%5d",coeff[i][j]);
    }
    printf("\n");
  }
}
