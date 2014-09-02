!  Module potts_ckbd_module and program potts_ckbd

!  Copyright by Claudio Rebbi  -  Boston University  -  April 2013
!  This software may be freely copied and used as long as this notice
!  is retained.
 

MODULE ising_ckbd_module

! This module defines parameters for the simulation of the 2-D ising
! model.

  IMPLICIT NONE

  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)

  INTEGER(LONG), PARAMETER :: MASK=281474976710655_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8
  INTEGER(LONG) :: seed_m, seed_a

  INTEGER, PARAMETER :: N=100  ! must be even

END MODULE ising_ckbd_module

PROGRAM ising_ckbd

! This program implements a Metropolis Monte Carlo simulation of the 
! 2 dimensional Ising model defined on a square lattice with periodic 
! boundary conditions.  The spins are stored in a single array, with
! the spins on the even lattice sites first and those on the odd lattice
! sites following.  This allows a parallel execution that takes advantage
! of OMP directives: the spins on the even sites are statistical independent
! from those on the odd sites and can be upgraded in parallel, and vice
! versa.  The size N must be even. 

! Note that the parallel execution requires that the random numbers
! be extracted in parallel.  This requires that the integers seed_m
! and seed_a used in the generation of random numbers be replaced
! with the numbers that correspond to the generation of the random
! seed which comes N**2/2 steps forward in the sequence.  This is 
! done by the subroutine set parallel rand, which receives the
! initial seed and returns an array of seeds which will be upgraded
! in parallel.


  USE ising_ckbd_module

  IMPLICIT NONE

  INTEGER, DIMENSION(0:N**2-1) :: s
  INTEGER, DIMENSION(0:1,0:N**2-1) :: xy ! x and y coordinates of s
  INTEGER, DIMENSION(0:N-1,0:N-1) :: index ! index of site of coord. x,y
  INTEGER, DIMENSION(4,0:N**2-1) :: nnindex ! index of nearest neighbor spins 
  INTEGER :: q,x,y,k,ka,nmeas,nit,meas,it,dir,i,xf,xb,yf,yb,cnt
  REAL(REAL8) :: beta,energy,acceptance,bf,r,mag,upgtime
  INTEGER(LONG) :: seed0
  INTEGER(LONG), DIMENSION(0:N**2/2-1) :: seed ! the seeds for the parallel
                                               ! upgrade of half the spins
  INTEGER :: news ! the tentative value of the upgraded spin
  LOGICAL, DIMENSION(0:N**2/2-1) :: accepted

  LOGICAL :: hot_start

  INTEGER :: rate,t0,t1,nupg  ! for timing information

  CALL SYSTEM_CLOCK(COUNT_RATE=rate)

  PRINT *
  WRITE(*,'("Clock rate:",I9," counts per second")'),rate
  PRINT *

  ! Build the array of coordinates and the relative index, 
  ! go through the even sites first

  k=0
  DO y=0,N-1
     DO x=0,N-1
        IF(MOD(x+y,2)==1) CYCLE
        xy(0,k)=x
        xy(1,k)=y
        index(x,y)=k
        k=k+1
     END DO
  END DO
   
  ! now go through the odd sites

  DO y=0,N-1
     DO x=0,N-1
        IF(MOD(x+y,2)==0) CYCLE
        xy(0,k)=x
        xy(1,k)=y
        index(x,y)=k
        k=k+1
     END DO
  END DO

  ! Build the array of nearest neighbor coordinates

  DO k=0,N**2-1
     x=xy(0,k)
     y=xy(1,k)
     xf=MOD(x+1,N)
     xb=MOD(x+N-1,N)
     yf=MOD(y+1,N)
     yb=MOD(y+N-1,N)
     nnindex(1,k)=index(xf,y) ! 1 is forward x
     nnindex(2,k)=index(xb,y) ! 2 is backward x
     nnindex(3,k)=index(x,yf) ! 3 is forward x
     nnindex(4,k)=index(x,yb) ! 4 is backward x
  END DO

  ! Modify to read these variables from input or from a file

  seed0=127_LONG
  hot_start=.TRUE.

  WRITE(*,'("Enter beta, nmeas, nit:  ")',ADVANCE='NO')
  READ *,beta,nmeas,nit
  PRINT *

  ! Initialize

  CALL set_parallel_rand(seed0,seed)

  IF(hot_start) THEN
     DO k=0,N**2/2-1
        seed(k)=IAND(seed_m*seed(k)+seed_a,MASK)
        s(k)=2*TWONEG48*seed(k)
!         PRINT *,seed(k)
     END DO
     DO k=N**2/2,N**2-1
        ka=k-N**2/2
        seed(ka)=IAND(seed_m*seed(ka)+seed_a,MASK)
!         PRINT *,seed(ka)
        s(k)=2*TWONEG48*seed(ka)
     END DO
  ELSE
     s=0
  ENDIF
 
  ! Execute measurements and Monte Carlo iterations between measurements

  DO meas=1,nmeas

     acceptance=0
    
     CALL SYSTEM_CLOCK(t0)

     DO it=1,nit   
       
        ! Metropolis upgrade, upgrade the even sites first, this can 
        ! be done in parallel

!$OMP PARALLEL DO PRIVATE(k,news,cnt,dir,bf,r), SHARED(s,nnindex,seed,accepted,beta,seed_m,seed_a)
        DO k=0,N**2/2-1

           news=1-s(k)  ! special to the Ising model

           ! Calculate the number of excited bonds after the change less the
           ! number of excited bonds before the change

           cnt=0
           DO dir=1,4 
              IF(s(nnindex(dir,k))/=news) cnt=cnt+1
              IF(s(nnindex(dir,k))/=s(k)) cnt=cnt-1
           END DO
           
           ! Calculate the ratioes of Boltzmann factors, accept or reject
           
           bf=EXP(-beta*cnt)
           
           ! Extract a random number 0<=r<1 with uniform prob. distribution:
           ! using the array seed(k) and the seed_m, seed_a factors which
           ! correspond to a step of N**2/2 in the congruantial pseudorandom
           ! number generator allows this operation to be parallelized

           seed(k)=IAND(seed_m*seed(k)+seed_a,MASK)
           r=TWONEG48*seed(k)
           accepted(k)=(r<=bf)  ! we do this to monitor the acceptance

           IF(accepted(k)) s(k)=news

        END DO

        acceptance=acceptance+COUNT(accepted)

        ! redo for the spins on the odd lattice

!$OMP PARALLEL DO PRIVATE(k,ka,news,cnt,dir,bf,r), SHARED(s,nnindex,seed,accepted,beta,seed_m,seed_a)
        DO k=N**2/2,N**2-1

           ka=k-N**2/2
           news=1-s(k)  ! special to the Ising model

           ! Calculate the number of excited bonds after the change less the
           ! number of excited bonds before the change

           cnt=0
           DO dir=1,4 
              IF(s(nnindex(dir,k))/=news) cnt=cnt+1
              IF(s(nnindex(dir,k))/=s(k)) cnt=cnt-1
           END DO
           
           ! Calculate the ratioes of Boltzmann factors, accept or reject

           bf=EXP(-beta*cnt)

           ! Extract a random number 0<=r<1 with uniform prob. distribution:
           ! using the array seed(k) and the seed_m, seed_a factors which
           ! correspond to a step of N**2/2 in the congruantial pseudorandom
           ! number generator allows this operation to be parallelized

           seed(ka)=IAND(seed_m*seed(ka)+seed_a,MASK)
           r=TWONEG48*seed(ka)
           accepted(ka)=(r<=bf)  ! we do this to monitor the acceptance

           IF(accepted(ka)) s(k)=news

        END DO

        acceptance=acceptance+COUNT(accepted)

     END DO

     CALL SYSTEM_CLOCK(t1)

     acceptance=acceptance/(N**2*nit)

     ! Measure the energy (only forward neighbors are needed) and the
     ! magnetization. This could be done in parallel
  
     energy=0
     energy=energy+COUNT(s/=s(nnindex(1,:)))
     energy=energy+COUNT(s/=s(nnindex(3,:)))
     mag=COUNT(s==0) 
     energy=energy/(2*N**2)
     mag=2*mag/(N**2)-1  ! 1 for s=0, -1 for s=1

     WRITE(*,'("At meas.",I5," energy=",F9.6," mag.=",&
          &F9.6," accept.=",F9.6)')meas,energy,mag,acceptance
     nupg=N**2*nit  ! number of spin upgrades (successful or attempted)
     upgtime=(REAL(t1-t0)/rate)/nupg  ! upgrade time per spin
     WRITE(*,'("Upgrade time per spin, in nanosec.:",F18.12)')upgtime*10.**9

     PRINT *

  END DO

END PROGRAM ising_ckbd

SUBROUTINE set_parallel_rand(seed0, seed)

! This subroutine receives the initial seed seed0, calculates the numbers 
! seed_m and seed_a which correspond to an advancement by N**2/2 steps
! of the congruential number generato, and fills and returns the array 
! of N**2/2 seeds which will be then upgraded in parallel in the main code.

  USE ising_ckbd_module

  IMPLICIT NONE

  INTEGER(LONG) :: seed_m0, seed_a0, c, aux
  INTEGER(LONG) :: seed0
  INTEGER(LONG), DIMENSION(0:N**2/2-1) :: seed 
  INTEGER :: k, i, m

  seed_m0=25214903917_LONG 
  seed_a0=11_LONG

  c=seed0

  DO k=0,N**2/2-1
     c=IAND(seed_m0*c+seed_a0,281474976710655_LONG)
     seed(k)=c
  END DO

  

  seed_m=1
  seed_a=0
  aux=N**2/2

  DO i=1,48
     m=IAND(aux,1)
     aux=ISHFT(aux,-1)
     IF(m.EQ.1) THEN
        seed_a=IAND(seed_m0*seed_a+seed_a0,281474976710655_LONG)
        seed_m=IAND(seed_m0*seed_m,281474976710655_LONG)
     ENDIF
     IF(aux.EQ.0) EXIT
     seed_a0=IAND(seed_m0*seed_a0+seed_a0,281474976710655_LONG)
     seed_m0=IAND(seed_m0*seed_m0,281474976710655_LONG)
  END DO

END SUBROUTINE set_parallel_rand
