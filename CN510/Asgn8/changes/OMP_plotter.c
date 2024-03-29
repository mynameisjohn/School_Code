#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "CNTools.h"

int genData(struct Neuron * dataset, T v){
   int i,j, sSum, max=0;
   T R, lam;
   lam = (1/v)*1000;

   srand((unsigned)time(0));
   for (i=0;i<N;i++){
      dataset[i].index=i;
      dataset[i].x=0;
      dataset[i].s=0;
      sSum=0;
      for (j=0;j<NSPIKE;j++){
         R=rand()/(T)(RAND_MAX);
         sSum += (int)(-1*lam*log(R));
         dataset[i].spikeTrain[j]=sSum;
      }
      if (sSum>max) max=sSum;
      dataset[i].w=rand()/(T)(RAND_MAX);
   }
  return max;
}

int main() {
   clock_t cstart = clock();
   clock_t cend = 0;

   FILE *output = fopen("./data/32/dat.out","w");
   FILE *weights = fopen("./data/32/weights.out","w");

   int time,i,j,tMax, nsp=0;
   T v=5.0, nu=0.1, A=50.0/1000.0, B=1.0, C=0.5, D=20.0/1000.0, Apost=25.0/1000, Cpost=0, tau, tauM=50.0, tauS=20.0;
   T xCum=0.0, V=0.0, dW=0.0;
   T oldWeights[N];

   struct Neuron pre[N];
   struct Neuron post;

   tau = (tauM*tauS)/(tauM-tauS);
  
   tMax = genData(pre,v);
   post.x=0;post.s=0;
   for (i=0;i<N;i++)
      oldWeights[i]=pre[i].w;

   for (time=0;time<tMax;time++) {
      fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",
         time, pre[0].x, pre[1].x, 
         xCum, V, post.x,
         pre[0].w, pre[1].w, dW);
      xCum=0;   
  
        for (i=0;i<N;i++) {
         dW=nu*pre[i].w*(pre[i].x*post.s-post.x*pre[i].s);
         pre[i].w += dW;

         for (j=0;j<NSPIKE;j++){
            pre[i].s = (pre[i].spikeTrain[j] == time);
            if (pre[i].s) break;
         }
         pre[i].x = solveRD(pre[i].x, A, pre[i].s*(B-C*pre[i].x));
         xCum += pre[i].x*pre[i].w;
      }
      xCum *= 2;    
     
       V = solveRD(V,D,
            xCum*tau*(solveRD(1,D,0)-solveRD(1,A,0)));

      post.s = (V>VTHRESH);
      if (post.s){
         V=VRESET;
         if (nsp<NSPIKE)
            post.spikeTrain[nsp++]=time;
      }
      post.x = solveRD(post.x,Apost,post.s*(B-Cpost*post.x));
   }

   for (i=0;i<N;i++)
      fprintf(weights,"%d\t%lf\t%lf\n",i,oldWeights[i],pre[i].w);
   
   fclose(output);
   fclose(weights);
   
   cend = clock();
   printf("The simulation took %.3f cpu seconds.\n", ((double)cend-(double)cstart) * 1.0e-6);

  return 1;
}
