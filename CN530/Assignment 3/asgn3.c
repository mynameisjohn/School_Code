#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define GRIDDIM 100
#define INPUTDIM 100
#define DT 0.0005

typedef double dType;

//Maximum of two values
dType max (dType a, dType b)
  {return (a<b) ? b : a;}

//Figure it out
dType min (dType a, dType b)
   {return (a>b) ? b : a;}

//Ensures m <= v <= M
dType clamp (dType v, dType m, dType M)
   {return min(max(v,m),M);}

// Rotter-Diessmann integration
dType solveRD (dType x0, dType a, dType b)
{
  return x0*exp(-a*DT)+b;
}

// Euler Advance
dType eulerAdvance(dType x0, dType v, dType dt)
{
  return x0 + dt * v;
}

typedef struct Layer{
   dType * input;
   dType * output;
   FILE * data;
   int (*solve)();
} Layer;

Layer * initLayer(dType * input, int (*layerFunc)(), char * fileName){
   Layer * layer = malloc(sizeof(Layer));
   layer->input = input;
   layer->solve = layerFunc;
   layer->data = fopen(fileName,"w");
   layer->output = calloc(GRIDDIM,sizeof(dType));
   return layer;
}

int closeLayer (Layer * layer){
   free(layer->output);
   fclose(layer->data);
   free(layer);
   return 1;
}

int printLayer(Layer * layer){
   int i;
   for (i=0;i<GRIDDIM;i++)
      fprintf(layer->data,"%d\t%lf\n",i,layer->output[i]);
   return 1;
}

int layerOneFunc(Layer * layer1){
   //layer1->output=layer1->input;
   int i;
   for (i=0;i<GRIDDIM;i++)
      layer1->output[i]=layer1->input[i];
   return 1;
}

int layerTwoFunc(Layer * layer2){
   dType sum1=0.0,sum2=0.0,A=1,B=90,C=4,D=60,E=0.5,alpha=1,beta=8.0;
   dType x;
   int i,j;

   alpha = -log(2.0)/(alpha*alpha);
   beta = -log(2.0)/(beta*beta);
   
   for (i=0;i<GRIDDIM;i++){
      for (j=0;j<GRIDDIM;j++){
	 dType r = (dType)((j-i)%GRIDDIM);
	 dType Ij = layer2->input[j];
	 r*=r;
	 sum1 += C*Ij*exp(alpha*r);
	 sum2 += E*Ij*exp(beta*r);
      }
      layer2->output[i]=max((B*sum1-D*sum2)/(A+sum1+sum2),0);
      sum1 = 0.0;
      sum2 = 0.0;
   }

   return 2;
}

int layerThreeFunc(Layer * layer3){
   dType F,G,H,gamma=3,n,sum1,Y,L=5;
   int i,j,k,K=2;

   gamma = -1.0/(gamma*gamma);

   //for every cell
   for (i=0;i<GRIDDIM;i++){
      //for every orientation
      for (k=0;k<K/2;k++){
	 sum1=0.0;
	 //find first half of clock EQ
	 for (j=0;j<GRIDDIM;j++){
	    dType r = (dType)(j-i);
	    dType Xj = layer3->input[j];
	    n = cos(2*M_PI*k/K);
	    //if (n>0.01) printf("%lf\n",n);
	    G=exp(gamma*r*r);
	    H=exp(gamma*(r-n)*(r-n));
	    F=(G-H);
	    sum1+=F*Xj;
	 }
	 Y=max(sum1,0);
	 //find second half of clock EQ
	 sum1=0.0;
         for (j=0;j<GRIDDIM;j++){
            dType r = (dType)(j-i);
            dType Xj = layer3->input[j];
            n = cos(2.0*M_PI*(k+K/2.0)/K);
            G=exp(gamma*r*r);
	    //if (n>0.01) printf("%lf\n",n);
            H=exp(gamma*(r-n)*(r-n));
            F=(G-H);
            sum1+=F*Xj;
         }
	 Y+=max(sum1,0);
//	 printf("%lf\n",G);
	 //Do the work of layer 4 and sum into layer 5
	 layer3->output[i]+=max(Y-L,0);
      }
   }
   
   return 3;
}

Layer * initLayerSix(Layer * layer3){
   dType P,M=10.0,delta=1000.0,epsilon=500.0;
   dType * X = layer3->input;
   dType * Z = layer3->output;
   dType * S = calloc(GRIDDIM,sizeof(dType));
   dType error;
   int i,s,nit=5000;
   FILE * output = fopen("relaxConverge.txt","w");

   for (s=0;s<nit;s++){
      error=0;
      for (i=0;i<GRIDDIM;i++){
         P = delta*(((i==0) ? 0.0 : 1.0/(1.0+epsilon*(Z[i-1]+Z[i]))) + 
             ((i==GRIDDIM) ? 0.0 : 1.0/(1.0+epsilon*(Z[i+1]+Z[i]))));
         dType tmp =  delta*(((i==0) ? 0.0 : S[i-1]/(1.0+epsilon*(Z[i-1]+Z[i]))) +
             ((i==GRIDDIM) ? 0.0 : S[i+1]/(1.0+epsilon*(Z[i+1]+Z[i]))));
         //if (P>0.01) printf("%lf\n",P);
         S[i]= (X[i] + tmp)/(M+P);
         //if (i==GRIDDIM/2) printf("%lf\n",S[i]-tmp);
	 //S[i]=tmp;
	//error+=(sqrt(pow(S[i]-S[i+1],2))+sqrt(pow(S[i]-S[i-1],2)))/2;
      }
      for (i=0;i<GRIDDIM;i++)
	 error+=(sqrt(pow(S[i]-S[i+1],2))+sqrt(pow(S[i]-S[i-1],2)))/2;
      //error+=(sqrt(pow(S[i]-S[i+1],2))+sqrt(pow(S[i]-S[i-1],2)))/2;
      fprintf(output,"%d\t%lf\n",s,error/GRIDDIM);
   }
   
   Layer * layer6 = initLayer(Z,layerOneFunc,"layerSix1.txt");
   layer6->output=S;
   fclose(output);

   return layer6;
}

Layer * initLayer6(Layer * layer3){
   dType delta=1000.0,epsilon=500.0,M=10.0,tmp;
   dType * S = calloc(GRIDDIM,sizeof(dType));
   dType * sDot = malloc(GRIDDIM*sizeof(dType));   
   dType * Z = layer3->output;
   dType * X = layer3->input;
   Layer * layer6 = initLayer(S,layerOneFunc,"layerSix2.txt");
   dType * sF = layer6->output;
   int i,t;
   dType error;
   FILE * output = fopen("trapConverge.txt","w");

   for (t=0;t<5000;t++){
      //error=0;
      for (i=0;i<GRIDDIM;i++){
	 sDot[i] = -M*S[i]+X[i]+delta*(
                                  ((i==0) ? 0 : (S[i-1]-S[i])/(1.0+epsilon*(Z[i-1]+Z[i]))) +
                                  ((i==GRIDDIM) ? 0 : (S[i+1]-S[i])/(1.0+epsilon*(Z[i+1]+Z[i]))));
	 sF[i]=eulerAdvance(S[i],sDot[i],DT);
      }
      for (i=0;i<GRIDDIM;i++){
	 sDot[i] += -M*sF[i]+X[i]+delta*(
	                                  ((i==0) ? 0 : (sF[i-1]-sF[i])/(1.0+epsilon*(Z[i-1]+Z[i]))) +
		                          ((i==GRIDDIM) ? 0 : (sF[i+1]-sF[i])/(1.0+epsilon*(Z[i+1]+Z[i]))));;
      }
      error=0;
      for (i=0;i<GRIDDIM;i++)
	 S[i]=eulerAdvance(S[i],sDot[i],DT/2.0);
      for (i=0;i<GRIDDIM;i++)
	 error+=(sqrt(pow(S[i]-S[i+1],2))+sqrt(pow(S[i]-S[i-1],2)))/2;
    //S[i]=tmp;
      
      fprintf(output,"%d\t%lf\n",t,error/GRIDDIM);
   }
   free(sDot);
   fclose(output);
   layer6->output=S;
   return layer6;
}

void convolve (dType * in, dType * out, int dir){
   dType sum1=0.0,sum2=0.0,A=4,b=1,C=0.5,d=8,decay=1,B=1,D=1;
   dType x;
   int i,j; dType z=((b*d*sqrt(log(A/C)))/(sqrt(d*d-b*b)));
  // printf("%lf\n",z);
   b=-1.0/(b*b);  
   d=-1.0/(d*d);   

   switch (dir){
      case 0:
         for (i=0;i<GRIDDIM;i++){
            for (j=i-(int)z-1;j>0;j--){
	       dType r = (dType)((j-i)%GRIDDIM);
	       dType Ij = in[j];
	       r*=r;
	       sum1 += A*Ij*exp(b*r);
	       sum2 += C*Ij*exp(d*r);
            }
            out[i]=max((-B*sum1+D*sum2)/(decay+sum1+sum2),0);
            sum1 = 0.0;
            sum2 = 0.0;
         }
         break;
      default:
         for (i=0;i<GRIDDIM;i++){
            for (j=i+(int)z+1;j<GRIDDIM;j++){
	       dType r = (dType)((j-i)%GRIDDIM);
	       dType Ij =in[j];
	       r*=r;
	       sum1 += A*Ij*exp(b*r);
	       sum2 += C*Ij*exp(d*r);
            }
            out[i]=max((-B*sum1+D*sum2)/(decay+sum1+sum2),0);
            sum1 = 0.0;
            sum2 = 0.0;
         }
         break;
   }
   return;
}

dType dotProd (dType * in, dType * out){
   float result=0;
   int i;
   for (i=0;i<GRIDDIM;i++)
      result+=(in[i]*out[i]);
   return result;
}

void partThree(dType * C){
   dType tauB=1,tauG=0.5,rho=1,gamma=1.5,dP, * Br, * Bl, * G, * tmp1, * tmp2, * tmp3, * tmp4;
   dType bDotR, bDotL, gDot;
   Br = malloc(sizeof(dType)*GRIDDIM);
   Bl = malloc(sizeof(dType)*GRIDDIM);
   G = malloc(sizeof(dType)*GRIDDIM);
   tmp1 = malloc(sizeof(dType)*GRIDDIM);
   tmp2 = malloc(sizeof(dType)*GRIDDIM);
   tmp3 = malloc(sizeof(dType)*GRIDDIM);
   tmp4 = malloc(sizeof(dType)*GRIDDIM);
   int i,t;

   FILE * output = fopen("partThree.txt","w");
   
   for (t=0;t<100;t++){
      convolve(Bl,tmp1,1);
      convolve(Br,tmp2,0);
      convolve(G,tmp3,1);
      convolve(G,tmp4,0);
      for (i=0;i<GRIDDIM;i++){
	 bDotR=(1.0/tauB)*(max(C[i]-rho*tmp3[i],0));
         bDotL=(1.0/tauB)*(max(C[i]-rho*tmp4[i],0));
	 gDot = (1.0/tauG)*(gamma*sqrt(tmp1[i]*tmp2[i]));

	 Br[i]=solveRD(Br[i],-1.0/tauB,bDotR);
	 Bl[i]=solveRD(Bl[i],-1.0/tauB,bDotL);
	 G[i]=solveRD(G[i],-1.0/tauG,gDot);
      }
      //fprintf(output,"%d\t%lf\t%lf\t%lf\n",t,Br[25],Bl[25],G[25]);
   }
   for (i=0;i<GRIDDIM;i++){
      fprintf(output,"%d\t%lf\t%lf\t%lf\t%lf\n",i,Br[i],Bl[i],G[i],(Br[i]-Bl[i])/(Br[i]+Bl[i]));
   }
   
   free(Br);
   free(Bl);
   free(G);
   free(tmp1);
   free(tmp2);
   free(tmp3);
   free(tmp4);
   fclose(output);
   return;
}

int main(){
   dType * I = malloc(sizeof(dType)*INPUTDIM);
   int i,j,k;

   for (i=0;i<INPUTDIM;i++){
    /*  if (i<10) I[i]=0;
      else if (i<90) I[i]=10;
      else I[i]=0;
    *//*  if (i<15) I[i]=20;
      else I[i]=3;
      */
      I[i]= ((i/10)%2 == 0) ? 10.0-(dType)(i/10) : 0.0;
/*
      if (i<5)
	 I[i]=0.0;
      else if (i<40)
         I[i]=10.0;
      else if (i<50)
	 I[i]=10+0.05*(dType)((i-40)*(i-40));
      else if (i<60)
	 I[i]=5+0.05*(dType)((i-50)*(i-50));
      else if (i<95)
	 I[i]=10.0;
      else
	 I[i]=0.0;
*//*
      if (i<10) I[i]=0;
      else if (i<50) I[i]=10;
      else if (i<90) I[i]=5;
      else I[i]=0;*/
   }

   clock_t begin = clock();

   Layer * layer1 = initLayer(I,layerOneFunc,"layerOne.txt");
   layerOneFunc(layer1);

   Layer * layer2 = initLayer(layer1->output,layerTwoFunc,"layerTwo.txt");
   layerTwoFunc(layer2);

   Layer * layer3 = initLayer(layer2->output,layerThreeFunc,"layerThree.txt");
   layerThreeFunc(layer3);

   Layer * layer61 = initLayer6(layer3);
   Layer * layer62 = initLayerSix(layer3);
   partThree(layer3->output);

   clock_t end = clock();

   printf("%lf\n",(double)(end-begin)/CLOCKS_PER_SEC);

   //partThree(layer3->output);

   printLayer(layer1);
   closeLayer(layer1);
   printLayer(layer2);
   closeLayer(layer2);
   printLayer(layer3);
   closeLayer(layer3);
   printLayer(layer61);
   closeLayer(layer61);
   printLayer(layer62);
   closeLayer(layer62);
   free(I);
   return 1;
}
