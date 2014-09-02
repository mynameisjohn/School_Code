#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#define DT 0.02

typedef double T;

T solveRD (T x0, T a, T b)
{
  return x0*exp(-a*DT)+b;
}

int main()
{
  srand((unsigned)time(0));
  T v,A,B,C,s;
  v = 5.0;
  A = 25.0/100;
  B = 1.0;
  C = 1;//1 with 46 and 86 0.5 with 82 81
  int tau = (int)(1000/A);
  printf("%d\n",tau);
  T *ex = malloc(sizeof(T)*20000);
  int *spikes = malloc(sizeof(int)*60);
  
  FILE *output;// = fopen("./data/64/dat.out","w");
  FILE *rasterData;// = fopen("./data/64/datR.out","w");
  char *outName = malloc(sizeof(char)*15);
  char *rasterName = malloc(sizeof(char)*16);
  if (sizeof(T)==4){
      strcpy(outName,"./data/32/dat__.out");
      strcpy(rasterName,"./data/32/datR__.out");
  }
  else{
    strcpy(outName,"./data/64/dat__.out");
      strcpy(rasterName,"./data/64/datR__.out");
  }

  T x=0,R; int e=0,i=0,n=0,spikeCount=60,currentTime=0,interval;
  /*for (i=0;i<60;i++)
    {
      R=rand()/(T)RAND_MAX;
      e += -200*log(R);//(1/v)*exp(-R/v);//pow(1/v,R)*exp(-v);///factorial(r);
      printf("%d\n",e);
    }
  //printf("%lf\n",ex/(T)i);
  */
  int sSum=0,tMax;
  for (n=1;n<=100;n++){
    outName[13]=(char)((int)'0'+n/10);rasterName[14]=(char)((int)'0'+n/10);
    outName[14]=(char)((int)'0'+n%10);rasterName[15]=(char)((int)'0'+n%10);
    output=fopen(outName,"w");
    rasterData=fopen(rasterName,"w");
    i=0;
    sSum=0;
  // initialize spike times
  while (i<spikeCount)
    {
      //printf("%d\n",i);
      R=rand()/(T)RAND_MAX;
      sSum += (int)(-200*log(R));
      spikes[i] = sSum;
      //printf("%d\n",sSum);
      i++;
      }
	
  // start the values off at 0
  i=0;
  fprintf(output,"%d\t%lf\n",currentTime,x);
  //fprintf(rasterData,"%d",n);
  
  while (i<spikeCount-1)
    {
      // go through all but the last spike
      currentTime=spikes[i]-1;
      fprintf(output,"%d\t%lf\n",currentTime,x);
      currentTime++;
      x=solveRD(x,A,B-C*x);
      fprintf(output,"%d\t%lf\n",currentTime,x);
      fprintf(rasterData,"%d\t%d\n",currentTime,n);
      interval = spikes[i+1]-spikes[i];
      ex[currentTime]+=x;
      tMax = (interval < tau*10) ? currentTime+interval : currentTime+tau*10;
      //printf("%d\n",currentTime);
      while (currentTime<tMax)
	{
	  ex[currentTime]+=x;
	  x=solveRD(x,A,0);
	  currentTime++; 
	  fprintf(output,"%d\t%lf\n",currentTime,x);
	}
       i++;
       //printf("%d\n",spikes[i]);
    }
  // for the last spike, let it decay and call it quits
  i=0;
  x=solveRD(x,A,B-C*x);
  ex[currentTime]+=x;
  fprintf(output,"%d\t%lf\n",currentTime,x);
  fprintf(rasterData,"%d\t%d\n",currentTime,n);
     
  while (i<tau*10)
    {
      x=solveRD(x,A,0);
      ex[currentTime]+=x;
      currentTime++; i++;
      fprintf(output,"%d\t%lf\n",currentTime,x);
    }
  
  
  /*
  while(i<spikeCount)
    {
      x=solveRD(x,A,B-C*x);
      printf("%lf\t%d\n",x,currentTime);
    
      interval=0;
      R=rand()/(T)RAND_MAX;
      e = -200*log(R);//(1/v)*exp(-R/v);//pow(1/v,R)*exp(-v);///factorial(r);
      //printf("%d\n",e);
      /*while (interval<e)
	{
	  x=solveRD(x,A,0);
	  printf("%lf\t%d\n",x,currentTime);
	  interval++;
	  currentTime++;
	  }*/
  /*currentTime+=e;
      if (e<10)
	while (interval<10)
	  {
	    x=solveRD(x,A,B-C*x);
	    printf("%lf\t%d",x,currentTime+interval);
	    interval++;
	  }
      else
	
      i++;
    }*/
  fclose(output);
  fclose(rasterData);
  }

  T V=0,D=10;

  output=fopen("./data/64/xCum1.dat","w");
  rasterData=fopen("./data/64/PSV.dat","w");
  /*for (i=0;i<20000;i++){
    fprintf(output,"%d\t%lf\n",i,ex[i]);
    fprintf(rasterData,"%d\t%lf\n",i,V);
    V=solveRD(V,D,0)+ex[i]*(100.0/30.0)*(exp(-D*DT)-exp(-A*DT));
    }*/
  fclose(output);
  free(ex);
  free(spikes);
  //alpha/beta function
  /* T y=0, dy=1;
  T t=0;
  A=50,B=0;
      output=fopen("./data/64/xCum1.dat","w");
  
while (t<1)
    {
      if (A==B)
	{//alpha
	  dy=exp(-A*DT)*(dy+B*y);
	  y=(DT*exp(-A*DT))*(dy+B*y)+y*exp(-A*DT);
	}
      else
	{//beta
	  dy=exp(-A*DT)*(dy+B*y);
	  y=(1/(B-A))*(exp(-A*DT)-exp(-B*DT))*(dy+B*y)+y*exp(-B*DT);
	}
      printf("%lf\t%lf\n",t,y);
      fprintf(output,"%lf\t%lf\n",t,y);
      t+=DT;
      }*/
  //fclose(output);
 return 1;
}