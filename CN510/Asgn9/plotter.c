/* 
   plotter.c
   A C program that plots a network of N neurons
   communicating to one post-synaptic cell over
   a period of time.

   John Joseph
   11/15/2013
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "CNTools.h"
#include <time.h>

#define DT 1
#define N 100
#define NSPIKE 60
#define VTHRESH 1000
#define VSPIKE  2000
#define VRESET -50

//The data Type used for simulation
typedef float T;

void output(char numTest, T *xCum1, T *xCum2, T *V, T *tr, int M)
{
  int t;
  // naming the output file
  FILE *output, *tOutput, *pOutput; char *fileName = malloc(sizeof(char)*14);
  if (sizeof(T)==4)
    strcpy(fileName,"./data/32/dat_.out");
  else
    strcpy(fileName,"./data/64/dat_.out");
  fileName[13]=numTest;
  output=fopen(fileName,"w");
  
  for (t=0;t<M;t++)
    fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\n",t,xCum1[t],xCum2[t],V[t],tr[t]);
  fclose(output);
}

int genData(struct Neuron * dataset, T v){
  int i,j, sSum, max=0;
  T R1, R2, X, Y, lam, jitter,sigma=3.0;
  lam = (1/v)*1000;

  srand((unsigned)time(0));

  for (i=0;i<10;i++){
    dataset[i].index=i;
    sSum=0;
    for (j=0;j<NSPIKE;j++){
      R1=rand()/(T)(RAND_MAX);
      
      sSum += (int)(-1*lam*log(R1));
      dataset[i].spikeTrain[j]=sSum;
      
      //if (i==42) printf("%d\n",dataset[i].spikeTrain[j]);
    }
    if (sSum>max) max=sSum;
    //printf("%d\t%d\n", max,i);

    // Randomize post-synaptic weights
    for (j=0;j<20;j++)
      dataset[i].weight[j]=rand()/(T)(RAND_MAX);
    //if (i==42) printf("%lf\n",dataset[i].weight);
  }
  
  
  for (i=10;i<N;i++){
    dataset[i].index=i;
    for (j=0;j<NSPIKE;j++){
      R1=rand()/(T)(RAND_MAX);
      R2=rand()/(T)(RAND_MAX);
      // generate random jitter and add it to the original spike train
      jitter=sigma*sqrt(-2*log(R1))*cos(2*M_PI*R2);
      
      dataset[i].spikeTrain[j]=dataset[i%10].spikeTrain[j] + jitter;
      //sSum;
      
      //if (i==42) printf("%d\n",dataset[i].spikeTrain[j]);
    }
    //printf("%d\t%d\n", max,i);
    

    for (j=0;j<20;j++)
      dataset[i].weight[j]=rand()/(T)(RAND_MAX);
    //if (i==42) printf("%lf\n",dataset[i].weight);
  }
  return max;
}

T * cumTrace1(struct Neuron *dataset, T A, T B, T C, int M, int p){
  int i,j, currentTime, interval, tau, tMax;
  T x, *xCum;
  
  tau = (int)(1/A);
  xCum = malloc(sizeof(T)*(M));
  for (i=0;i<M;i++) xCum[i]=0;
  
    for (i=0;i<N;i++){
      x=0.0;
      for (j=0;j<NSPIKE-1;j++){
	currentTime=dataset[i].spikeTrain[j];
	x=solveRD(x,A,B-C*x);
	xCum[currentTime]+=2*dataset[i].weight[p]*x;
	interval = dataset[i].spikeTrain[j+1]-dataset[i].spikeTrain[j];
	tMax = (interval < tau*10) ? currentTime+interval : currentTime+tau*10;
	while (currentTime<tMax)
	  {
	    currentTime++;
	    x=solveRD(x,A,0);
	    xCum[currentTime]+=2*dataset[i].weight[p]*x;
	    currentTime++; 
	    //fprintf(output,"%d\t%lf\n",currentTime,x);
	  }
      }/*
	 j=0;
	 x=solveRD(x,A,B-C*x);
	 xCum[currentTime]+=2*dataset[i].weight*x;
	 while (j<tau*10)
	 {
	 x=solveRD(x,A,0);
	 xCum[currentTime]+=2*dataset[i].weight*x;
	 currentTime++;j++;
	 //fprintf(output,"%d\t%lf\n",currentTime,x);
	 }*/
    }
    return xCum;
  
}

T * postSynaptic(T *xCum,T tauS, T tauM, T A, T D, int M, struct Neuron post){
  int i,j;
  T *V = malloc(sizeof(T)*M);
  int ns=0;

  for (i=0;i<M;i++) V[i]=0;
  
  //printf("%d\n",M);
  for (i=0;i<M-1;i++&&ns<=NSPIKE){
    if (V[i]==VSPIKE) V[i+1]=VRESET;
    else{
      V[i+1]=solveRD(V[i],D,0)+xCum[i]*((tauS*tauM)/(30))*(solveRD(1,D,0)-solveRD(1,A,0));
      if (V[i+1]>VTHRESH) {
	V[i+1]=VSPIKE;ns++;post.spikeTrain[ns]=i;
      }
    }
  }
  return V;
}

T * trace(struct Neuron neuron, T A, T B, T C, int M){
  int j, currentTime, interval, tau, tMax;
  T *x = malloc(sizeof(T)*M);
  for (j=0;j<M;j++) x[j]=0.0;
  tau = (int)(1/A);
  
  for (j=0;j<NSPIKE-1;j++){
    // get the time of the next spike spike, go right before
    currentTime=neuron.spikeTrain[j]-1.0;
    x[currentTime+1]=solveRD(x[currentTime],A,B-C*x[currentTime]);
    interval = neuron.spikeTrain[j+1]-neuron.spikeTrain[j];
    tMax = (interval < tau*10) ? currentTime+interval : currentTime+tau*10;
    while (currentTime<tMax)
      {
	currentTime++;
	x[currentTime+1]=solveRD(x[currentTime],A,0.0);
      }
  }    
  return x;
}

int main()
{
  int tau,M,i;
  struct Neuron dataset[N];
  T v=5.0, A=50.0/1000.0, B=1.0, C=0.5, D=10/1000.0, tauS=20,tauM=50, *cum1, *cum2, *V, *tr, *w1, *w2;

  FILE *postOut;

  struct Neuron *post=malloc(sizeof(struct Neuron)*20);
  
  tau = (int)(1/A);
  
  M = genData(dataset,v)+200;
  printf("%d\n",M);
  // get data for the 0th post-synaptic cell
  cum1 = cumTrace1(dataset,A,B,C,M,0);
  V = postSynaptic(cum1,tauS,tauM,A,D,M,post[0]);
  postOut = fopen("./data/32/postOut1.out","w");
  for (i=0;i<M;i++)
    fprintf(postOut,"%d\t%lf\n",i,V[i]);
  fclose(postOut);

  // and the 1st
  cum1 = cumTrace1(dataset,A,B,C,M,1);
  V = postSynaptic(cum1,tauS,tauM,A,D,M,post[1]);
  postOut = fopen("./data/32/postOut2.out","w");
  for (i=0;i<M;i++)
    fprintf(postOut,"%d\t%lf\n",i,V[i]);
  fclose(postOut);
  
  // don't really need to do this, already got data from first two
  for (i=2;i<20;i++){
    cum1 = cumTrace1(dataset,A,B,C,M,i);
    V = postSynaptic(cum1,tauS,tauM,A,D,M,post[i]);
  }

  free(cum1);
  free(cum2);
  free(V);
  //printf("%lf\n",dataset[42].weight);
  //learn(dataset,post,A,B,C,M);
  

  //output('1',cum1,cum2,V,tr,M);
  
  free(post);

  return 1;
}