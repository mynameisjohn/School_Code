!  Module ising_module and program ising

!  Copyright by Claudio Rebbi  -  Boston University  -  December 2003
!  This software may be freely copied and used as long as this notice
!  is retained.
 

MODULE ising_module

! This module defines parameters for the simulation of the 2-D Ising
! model.

  IMPLICIT NONE

  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)

  INTEGER(LONG) :: seed_m=25214903917_LONG, seed_a=11_LONG,&
       mask=281474976710655_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8

  INTEGER, PARAMETER :: N=100

END MODULE ising_module

PROGRAM ising

! This program implements a Metropolis Monte Carlo simulation of the 
! 2 dimensional ising model defined on a square lattice with periodic 
! boundary conditions.  Although serial, the code is written in such
! a way that the spins on the even sites are upgraded first, then
! those on the odd sites.  Thus 

! The program is rather streamlined.  For better functionality it should
! be modified to read from input also the variables q, saved_seed and
! hot_start and to save output data on a file in a format suitable for 
! further manipulations.
 
! It would also be useful to implement the capability of saving the 
! final spin configuration and pseudorandom number seed on a file 
! and of restarting the simulation from these data.

! These modifications are left to the student.

  USE ising_module

  IMPLICIT NONE

  INTEGER, DIMENSION(0:N-1,0:N-1) :: s
  INTEGER, DIMENSION(4) :: nns
  INTEGER x,y,nmeas,nit,meas,it,dir,i,xf,xb,yf,yb,news,cnt
  REAL(REAL8) beta,energy,acceptance,bf,r,mag
  INTEGER(LONG) seed
  LOGICAL hot_start
  
  ! Modify to read these variables from input or from a file
  
  seed=127_LONG
  hot_start=.TRUE.
  
  WRITE(*,'("Enter beta, nmeas, nit:  ")',ADVANCE='NO')
  READ *,beta,nmeas,nit
  
  ! Initialize
  
  ! this makes the sequence of seeds
  ! identical to the one in ising_checkerboard
  DO i=0,N**2/2-1
     seed=IAND(seed_m*seed+seed_a,mask)  
  END DO

  ! in a hot start we extract the spins on the even and odd sites
  ! separately to be in agreement with the prograb ising_checkerboard
  IF(hot_start) THEN
     DO y=0,N-1
        DO x=0,N-1
           IF(MOD(x+y,2)==1) CYCLE
           seed=IAND(seed_m*seed+seed_a,mask)
!            PRINT *,seed
           s(x,y)=2*TWONEG48*seed
        END DO
     END DO
     DO y=0,N-1
        DO x=0,N-1
           IF(MOD(x+y,2)==0) CYCLE
           seed=IAND(seed_m*seed+seed_a,mask)
!            PRINT *,seed
           s(x,y)=2*TWONEG48*seed
        END DO
     END DO
  ELSE
     s=0
  ENDIF
  

  ! Execute measurements and Monte Carlo iterations between measurements

  DO meas=1,nmeas
     
     acceptance=0
     
     DO it=1,nit   
        
        ! Metropolis upgrade, done serially through the lattice,
        ! the spins at the even sites are upgraded first

        DO y=0,N-1
           
           yf=y+1
           IF(yf==N) yf=0
           yb=y-1
           IF(yb==-1) yb=N-1
           
           DO x=0,N-1

              IF(MOD(x+y,2)==1) CYCLE               

              xf=x+1
              IF(xf==N) xf=0
              xb=x-1
              IF(xb==-1) xb=N-1
              
              ! Gather nearest neighbors, 1 is forward x, 2 is backward x, 
              ! 3 is forward y, 4 is backward y
              
              nns(1)=s(xf,y)
              nns(2)=s(xb,y)
              nns(3)=s(x,yf)
              nns(4)=s(x,yb)
              
              ! Preselect a new value for the spin
              
              news=1-s(x,y)
              
              ! Calculate the number of excited bonds after the change 
              ! less the number of excited bonds before the change

              cnt=0
              DO dir=1,4 
                 IF(nns(dir)/=news) cnt=cnt+1
                 IF(nns(dir)/=s(x,y)) cnt=cnt-1
              ENDDO
              
              ! Calculate the ratioes of Boltzmann factors, accept or reject
              
              bf=EXP(-beta*cnt)
              
              ! Extract a random number 0<=r<1 with uniform prob. distribution

              seed=IAND(seed_m*seed+seed_a,mask)
              r=TWONEG48*seed

              IF(r<=bf) THEN

                 s(x,y)=news

                 ! Monitor the acceptance      
                 
                 acceptance=acceptance+1

              ENDIF
              
           END DO

        END DO

        ! now we upgrade the spins on the odd sites

        DO y=0,N-1
           
           yf=y+1
           IF(yf==N) yf=0
           yb=y-1
           IF(yb==-1) yb=N-1
           
           DO x=0,N-1

              IF(MOD(x+y,2)==0) CYCLE               

              xf=x+1
              IF(xf==N) xf=0
              xb=x-1
              IF(xb==-1) xb=N-1
              
              ! Gather nearest neighbors, 1 is forward x, 2 is backward x, 
              ! 3 is forward y, 4 is backward y
              
              nns(1)=s(xf,y)
              nns(2)=s(xb,y)
              nns(3)=s(x,yf)
              nns(4)=s(x,yb)
              
              ! Preselect a new value for the spin
              
              news=1-s(x,y)
              
              ! Calculate the number of excited bonds after the change 
              ! less the number of excited bonds before the change

              cnt=0
              DO dir=1,4 
                 IF(nns(dir)/=news) cnt=cnt+1
                 IF(nns(dir)/=s(x,y)) cnt=cnt-1
              ENDDO
              
              ! Calculate the ratioes of Boltzmann factors, accept or reject
              
              bf=EXP(-beta*cnt)
              
              ! Extract a random number 0<=r<1 with uniform prob. distribution

              seed=IAND(seed_m*seed+seed_a,mask)
              r=TWONEG48*seed

              IF(r<=bf) THEN

                 s(x,y)=news

                 ! Monitor the acceptance      
                 
                 acceptance=acceptance+1

              ENDIF
              
           END DO

        END DO

     END DO

     acceptance=acceptance/(N*N*nit)

! Measure the energy (only forward neighbors are needed) and magnetization
! This can be done in parallel

     energy=0
     mag=0

     energy=energy+COUNT(s/=CSHIFT(s(:,:),1,1))
     energy=energy+COUNT(s/=CSHIFT(s(:,:),1,2))
     mag=COUNT(s(:,:)==0) 

     energy=energy/(2*N*N)
     mag=2*mag/(N*N)-1

     WRITE(*,'("At meas.",I5," energy=",F9.6," mag.=",&
          &F9.6," accept.=",F9.6)')meas,energy,mag,acceptance
     PRINT *

  END DO

END PROGRAM ising
