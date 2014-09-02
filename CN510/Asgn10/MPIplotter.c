#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "CNTools.h"
#include <mpi.h>

int genData(struct Neuron * dataset, T v){

   int i,j, sSum, max=0;
   T R1, R2, lam, jitter;
   lam = (1/v)*1000;

   srand((unsigned)time(0));

   for (i=0;i<10;i++){
      dataset[i].index=i;
      dataset[i].x=0;
      dataset[i].s=0;
      sSum=0;
      for (j=0;j<20;j++){
         sSum += (int)(-1*lam*log(R1));
         dataset[i].spikeTrain[j]=sSum;
      }
      if (sSum>max) max=sSum;
   }

   for (i=10;i<N;i++){
      dataset[i].index=i;
      dataset[i].x=0;
      dataset[i].s=0;
      sSum=0;
      for (j=0;j<NSPIKE;j++){
         R1=rand()/(T)(RAND_MAX);
         R2=rand()/(T)(RAND_MAX);
         jitter=sqrt(-2*log(R1))*cos(2*M_PI*R2);
         dataset[i].spikeTrain[j]=dataset[i%10].spikeTrain[j]+jitter;
//	printf("%lf\n",jitter);
      }
      if (dataset[i].spikeTrain[j]>max) max=dataset[i].spikeTrain[j];
   }

   for (i=0;i<N;i++)
      for (j=0;j<20;j++)
         dataset[i].w[j]=rand()/(T)(RAND_MAX);

  return max;
}

int main(int argc, char *argv[]) {
   /*FILE *output = fopen("./data/32/dat.out","w");
   FILE *weights = fopen("./data/32/weights.out","w");
   FILE *oldWeights = fopen("./data/32/oldWeights.out","w");
   FILE *postWeights;// = fopen("./data/32/postWeights_.out","w");
   FILE *trace = fopen("./data/32/trace.out","w");*/
   
   int nprocs,rank,master=0;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&rank);
   MPI_Comm_size(MPI_COMM_WORLD,&nprocs);

   int localSize = N/(nprocs-1);

   int time,i,j,k[25],tMax, count=0,nsp=0;
   T v=5.0, nu=200.0/1000.0, A=50.0/1000.0, B=1.0, C=0.5, D=20.0/1000.0; 
   T Apost=25.0/1000, Cpost=0, tau, tauM=50.0, tauS=20.0, inhib;
   T xCum[20],hostCum[20], V[20];
   T dW[20*N];

   for (j=0;j<20;j++)
      xCum[j]=V[j]=0;

   struct Neuron pre[localSize];
   struct Neuron post[20];

   tau = (tauM*tauS)/(tauM-tauS);
  
   tMax = genData(pre,v);
   for (j=0;j<20;j++){
      post[j].x=0;
      post[j].s=0;
   }


   for (i=0;i<N;i++)
      for (j=0;j<20;j++){
         k[((int)(pre[i].w[j]*100.0))/4]++;
      }	
   //for (i=0;i<25;i++)
      //fprintf(oldWeights,"%d\t%d\n",i,k[i]);
   //fclose(oldWeights);

   for (time=0;time<tMax;time++) {
      /*fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",
         time, V[5], V[10],
         pre[0].x,post[0].x,pre[0].w[0],
         pre[1].x,post[1].x,pre[1].w[1],
         pre[2].x,post[2].x,pre[2].w[2],
         pre[3].x,post[3].x,pre[3].w[3],
         pre[4].x,post[4].x,pre[4].w[4],
         pre[5].x,post[5].x,pre[5].w[5],
         pre[6].x,post[6].x,pre[6].w[6],
         pre[7].x,post[7].x,pre[7].w[7],
         pre[8].x,post[8].x,pre[8].w[8],
         pre[9].x,post[9].x,pre[9].w[9],
         pre[10].x,post[10].x,pre[10].w[10]);*/
/*

pre[0].x, pre[1].x, 
         xCum[0], V[0], post[0].x,
         xCum[1], V[1], post[1].x,
         xCum[2], V[2], post[2].x,
         xCum[3], V[3], post[3].x,
         xCum[4], V[4], post[4].x,
         xCum[5], V[5], post[5].x,
         xCum[6], V[6], post[6].x,
         xCum[7], V[7], post[7].x,
         xCum[8], V[8], post[8].x,
         xCum[9], V[9], post[9].x);
  */ 
      /*for (j=0;j<20;j++)
         dW[i+j] = nu*(pre[i].x*post[j].s-post[j].x*pre[i].s);      
   */
      if (rank==master) {
         for (j=0;j<20;j++)
            hostCum[j]=0;
      }
      else{
         for (j=0;j<20;j++)
            xCum[j]=0;
      
      for (i=0;i<localSize;i++) {
         for (j=0;j<NSPIKE;j++){
            pre[i].s = (pre[i].spikeTrain[j] == time);
            if (pre[i].s) break;
         }
         for (j=0;j<20;j++)
            dW[20*i+j] = nu*(pow((1.0-pre[i].w[j]),0.8)*pre[i].x*post[j].s-pow(pre[i].w[j],0.8)*post[j].x*pre[i].s);    

         pre[i].x = solveRD(pre[i].x, A, pre[i].s*(B-C*pre[i].x));

         for (j=0;j<20;j++){
            xCum[j] += pre[i].x*pre[i].w[j];
            //dW[i+j] = nu*(pre[i].x*post[j].s-post[j].x*pre[i].s);
         }
      }
}
      MPI_Reduce(&xCum,&hostCum,20,MPI_FLOAT,MPI_SUM,master,MPI_COMM_WORLD);

   if (rank==master){
//Add inhibitory terms
      for (j=0;j<20;j++){
         inhib=0;
         xCum[j]*=2;  
         for (i=0;i<20;i++)
            if (i!=j) inhib+=post[i].x;
   
         V[j] = solveRD(V[j],D,(xCum[j]-inhib)*
            tau*(solveRD(1,D,0)-solveRD(1,A,0)));

         post[j].s = (V[j]>VTHRESH);
         if (post[j].s)
            V[j]=VRESET;
      }
      for (j=0;j<20;j++)
         post[j].x = solveRD(post[j].x,Apost,
            post[j].s*(B-Cpost*post[j].x));
      
      for (i=0;i<N;i++)
         for (j=0;j<20;j++){
            pre[i].w[j] = clamp((pre[i].w[j]+dW[20*i+j]),(T)0,(T)1);
            //if (pre[i].w[j]==(T)1) count++; //printf("durr durr durr\n");
         }
      }
   }
  /* 
   FILE *rasterPlot=fopen("./data/32/rasterPlot.out","w");
   fclose(output);
  //printf(spikeTime index tab) 
   for (j=0;j<NSPIKE;j++){
      for (i=0;i<N;i++)
         fprintf(rasterPlot,"%d\t%d\t",pre[i].spikeTrain[j],pre[i].index);
      fprintf(rasterPlot,"\n");
   }
   fclose(rasterPlot);
*/

//   fclose(output);	
/*   for (i=0;i<20;i++){

      
      for (j=0;j<N;j++)
         fprintf("%d\t%d\t%lf\n",i,j,(pre[i].w[j]));//==(T)1) count++;
   }*/

   for (i=0;i<N;i++)
      for (j=0;j<20;j++){
         k[((int)(pre[i].w[j]*100.0))/4]++;
      } 
   for (i=0;i<25;i++)
      //fprintf(weights,"%d\t%d\n",i,k[i]);
   //fclose(weights);
   
   MPI_Finalize();
   return 1;
}
