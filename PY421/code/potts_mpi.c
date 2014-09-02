/* This program provides a multi-processor message passing implementation 
   of the Metropolis Monte Carlo simulation of the q-state two-dimensional 
   Potts model.

   The spins are distributed among NP processors, where they are stored in
   local arrays s[M+2][NY].  We assume that M is even.  Let us denote by x
   and y the integer valued indices of these arrays and by k (0<=k<NP) a
   further index labeling the processors.  The entire system consists of
   NX=M*NP times NY spins, distributed among the processors and stored in
   the local arrays s.  These spins will never be assembled into a single
   array, but conceptually it is convenient to think of them as belonging
   to a global array with integer valued indices xg (0<=xg<NX) and y
   (0<=y<NY).  The y index is the same as the index of the local spin
   arrays, and will be dropped from further consideration.  The relation
   between the local x indices and the global index xg is given by
   xg=x-1+k*M (the offset by -1 is motivated by the fact that all arrays
   are 0 based and that we will need to store in the array s two extra
   columns, one to the left of the first column and one to the right of
   the last one).  The columns with the same x index (with 1<=x<=M)
   are upgraded in parallel by all processors in such a way that the
   upgrades of columns 1 and M are separated by an equal number of column
   upgrades (this in order to leave the maximum time for transmitting the
   data). After their upgrade, the spins in the column with x=M are
   copied into the column with x=0 of the next processor (with cyclic
   ordering) and those in the column with x=1 are copied into the column
   with x=M+1 of the previous processor (always with cyclic ordering).
   Synchronization barriers insure that these data are received before
   they are needed for the upgrades of columns 1 and M, respectively.  In
   this manner each processor has always a current image of all the spins
   ranging from the column to the left of the first column it must upgrade
   to the column to the right of the last column it must upgrade, and
   all the upgrades can be done locally and in parallel.


              
                                       Claudio Rebbi and Francis Starr
                                       Boston University, April 1996
                                                                       */
                         
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpi.h"

#define M 20
#define NY 20
#define QMAX 100

main(argc, argv)
int argc;
char **argv;
{
  int s[M+2][NY],snew;
  int x,y,i,ynn;
  int q,nupg,nmeas,upg,meas;
  double beta,den,energy,mag[QMAX],accept;
  unsigned short seed[3];
  /* MPI variables */
  double etotal, magtotal[QMAX], accept_tot;
  int rank, NP, left, right;
  MPI_Status status;

/* MPI Initialization */
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &NP);

/* The root process prints out the lattice size, reads in parameters 
   and broadcasts to all */
  if (rank == 0) {
    printf("The lattice size is %d by %d.\n",M*NP,NY); 
    printf("Enter the number of states q and the value of beta:\n");
    scanf("%d%lf",&q,&beta);
    printf("Enter the number of measurements and the number of upgrades\n");
    printf("between measurements:\n");
    scanf("%d%d",&nmeas,&nupg);
  }
/* Broadcast parameters from process 0 to all others.  
   This is a blocking operation */
  MPI_Bcast(&q, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&beta, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
  MPI_Bcast(&nmeas, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&nupg, 1, MPI_INT, 0, MPI_COMM_WORLD);

/* Identity of process that is left and right of this process.  We use
   mod to obtain the cylindrical boundary conditions. */
  right = (rank+1)%NP;
  left  = (rank-1+NP)%NP;
  
/* Initialize cold */

  for(x=1;x<=M;x++){
    for(y=0;y<NY;y++) {
      s[x][y]=0;
    }
  }
     
/* Seed for random numbers.  Add the rank of the process to the seed to
   obtain different radom numbers for each process */
  seed[0]=123+rank; seed[1]=5; seed[2]=27;

/* Multiprocessing part */

/* Send the first column to the previous processor in cyclic order, 
   this probably does not require blocking */

  MPI_Send(s[1], NY, MPI_INT, left, 99, MPI_COMM_WORLD);
  
/* Receive and store in column M+1.  This should receive what has 
   been sent above, but probably does not require blocking either, 
   just the availability of the data */

  MPI_Recv(s[M+1], NY, MPI_INT, right, 
	   99, MPI_COMM_WORLD, &status);

/* Perform the measurement cycle */

  for(meas=1;meas<=nmeas;meas++){
    
    accept=0;

    for(upg=1;upg<=nupg;upg++){
      
/* Upgrade of the even columns. Up to x=M-2 this can be concurrent with
   the send and receive initiated at the end of the loop over upgrades */
      
      for(x=2;x<=M;x+=2){

	if(x==M){
	  
/* Synchronization barrier for x=M.  The upgrade of the Mth column cannot 
   proceed until the transmitted data have been stored in column M+1 */
	  MPI_Barrier(MPI_COMM_WORLD);
	}
	
	for(y=0;y<NY;y++){
	  snew=(q-1)*erand48(seed)+1;
	  snew=(s[x][y]+snew)%q;
	  den=0;
	  if(snew!=s[x+1][y]) den++;
	  if(s[x][y]!=s[x+1][y]) den--;
	  if(snew!=s[x-1][y]) den++;
	  if(s[x][y]!=s[x-1][y]) den--;
	  ynn=(y+1)%NY;
	  if(snew!=s[x][ynn]) den++;
	  if(s[x][y]!=s[x][ynn]) den--;
	  ynn=(y+NY-1)%NY;
	  if(snew!=s[x][ynn]) den++;
	  if(s[x][y]!=s[x][ynn]) den--;
	  if(erand48(seed)<=exp(-beta*den)) {
	    s[x][y]=snew;
	    accept++;
	  }
	}
      }
      
/* Send the Mth column to the next processor in cyclic order, 
   this probably does not require blocking */

      MPI_Send(s[M], NY, MPI_INT, right, 99, MPI_COMM_WORLD);

/* Receive and store in column 0.  This should receive what has been 
   sent above, but probably does not require blocking either, just 
   the availability of the data */

      MPI_Recv(s[0], NY, MPI_INT, left, 
	       99, MPI_COMM_WORLD, &status);
      
/* Upgrade odd columns from 3 to M-1, can be concurrent with the send 
   and receive just above */

      for(x=3;x<M;x+=2){
	for(y=0;y<NY;y++){
	  snew=(q-1)*erand48(seed)+1;
	  snew=(s[x][y]+snew)%q;
	  den=0;
	  if(snew!=s[x+1][y]) den++;
	  if(s[x][y]!=s[x+1][y]) den--;
	  if(snew!=s[x-1][y]) den++;
	  if(s[x][y]!=s[x-1][y]) den--;
	  ynn=(y+1)%NY;
	  if(snew!=s[x][ynn]) den++;
	  if(s[x][y]!=s[x][ynn]) den--;
	  ynn=(y+NY-1)%NY;
	  if(snew!=s[x][ynn]) den++;
	  if(s[x][y]!=s[x][ynn]) den--;
	  if(erand48(seed)<=exp(-beta*den)) {
	    s[x][y]=snew;
	    accept++;
	  }
	}
      }
      
/* Synchronization barrier.  The upgrade of the first column cannot
   proceed until the transmitted data have been copied into column 0 */
      MPI_Barrier(MPI_COMM_WORLD);

/* Upgrade the first column */

      x=1;

      for(y=0;y<NY;y++){
	snew=(q-1)*erand48(seed)+1;
	snew=(s[x][y]+snew)%q;
	den=0;
	if(snew!=s[x+1][y]) den++;
	if(s[x][y]!=s[x+1][y]) den--;
	if(snew!=s[x-1][y]) den++;
	if(s[x][y]!=s[x-1][y]) den--;
	ynn=(y+1)%NY;
	if(snew!=s[x][ynn]) den++;
	if(s[x][y]!=s[x][ynn]) den--;
	ynn=(y+NY-1)%NY;
	if(snew!=s[x][ynn]) den++;
	if(s[x][y]!=s[x][ynn]) den--;
	if(erand48(seed)<=exp(-beta*den)) {
	  s[x][y]=snew;
	  accept++;
	}
      }

/* Send the first column to the previous processor in cyclic order, 
   this probably does not require blocking */

      MPI_Send(s[1], NY, MPI_INT, left, 99, MPI_COMM_WORLD);

/* Receive and store in column M+1.  This should receive what has 
   been sent above, but probably does not require blocking either, 
   just the availability of the data */

      MPI_Recv(s[M+1], NY, MPI_INT, right, 
	       99, MPI_COMM_WORLD, &status);
      
    }

/* Measure acceptance, energy and magnetization locally, 
   then broadcast and add */
    
    accept/=M*NY*nupg;
    accept_tot=0;
    energy=etotal=0;

    for(i=0;i<q;i++) mag[i]=magtotal[i]=0;

/* We sum the energy starting from column 0, because the synchronization 
   barrier in the middle of the loop over upgrades guarantees that the
   data for column 0 have been received */

    for(x=0;x<M;x++){     
      for(y=0;y<NY;y++){
        ynn=(y+1)%NY;
	if(s[x][y]!=s[x+1][y]) energy++;
	if(s[x][y]!=s[x][ynn]) energy++;
        mag[s[x][y]]++;        
      }
    }

    energy/=2*M*NY;
    for(i=0;i<q;i++) mag[i]/=M*NY;

/* Now must reduce our data to give it to the user */
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Reduce(&energy, &etotal, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(mag, magtotal, q, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(&accept, &accept_tot, 1, MPI_DOUBLE, MPI_SUM, 0, 
	       MPI_COMM_WORLD);
    
/* only root process prints out data */
    if (rank == 0) {
      etotal /= NP;
      accept_tot /= NP;
      for (i=0; i<q; i++)
	magtotal[i] /= NP;
      printf("At meas. %d energy= %f, acceptance= %f\n", 
	     meas, etotal, accept_tot);
      printf("magnetizations: ");
      for(i=0;i<q;i++) printf("%f ",magtotal[i]);
      printf("\n\n"); 
    }
  }
  MPI_Finalize();
}
