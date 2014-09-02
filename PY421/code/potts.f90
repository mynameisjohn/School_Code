!  Module potts_module and program potts

!  Copyright by Claudio Rebbi  -  Boston University  -  December 2003
!  This software may be freely copied and used as long as this notice
!  is retained.
 

MODULE potts_module

! This module defines parameters for the simulation of the 2-D Potts
! model.

  IMPLICIT NONE

  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)

  INTEGER(LONG) :: seed_m=25214903917_LONG, seed_a=11_LONG,&
       mask=281474976710655_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8

  INTEGER, PARAMETER :: NX=32, NY=32

END MODULE potts_module

PROGRAM potts

! This program implements a Metropolis Monte Carlo simulation of the 
! 2 dimensional Potts model defined on a square lattice with periodic 
! boundary conditions.

! The program is rather streamlined.  For better functionality it should
! be modified to read from input also the variables q, saved_seed and
! hot_start and to save output data on a file in a format suitable for 
! further manipulations.
 
! It would also be useful to implement the capability of saving the 
! final spin configuration and pseudorandom number seed on a file 
! and of restarting the simulation from these data.

! These modifications are left to the student.

  USE potts_module

  IMPLICIT NONE

  INTEGER, DIMENSION(0:NX-1,0:NY-1) :: s
  INTEGER, DIMENSION(4) :: nns
  INTEGER q,x,y,nmeas,nit,meas,it,dir,i,xf,xb,yf,yb,news,cnt
  REAL(REAL8) beta,energy,acceptance,bf,r
  INTEGER(LONG) seed
  REAL(REAL8), DIMENSION(:), ALLOCATABLE :: mag
  LOGICAL hot_start

! Modify to read these variables from input or from a file

  q=6
  seed=127_LONG
  hot_start=.TRUE.

  WRITE(*,'("Enter beta, nmeas, nit:  ")',ADVANCE='NO')
  READ *,beta,nmeas,nit

! Initialize

  ALLOCATE(mag(0:q-1))

  IF(hot_start) THEN
     DO x=0,NX-1
        DO y=0,NY-1
           seed=IAND(seed_m*seed+seed_a,mask)
           s(x,y)=q*TWONEG48*seed
        END DO
     END DO
  ELSE
     s=0
  ENDIF
  
! Execute measurements and Monte Carlo iterations between measurements

  DO meas=1,nmeas
     
     acceptance=0
     
     DO it=1,nit   
        
! Metropolis upgrade, done serially through the lattice

        DO x=0,NX-1

           xf=x+1
           IF(xf==NX) xf=0
           xb=x-1
           IF(xb==-1) xb=NX-1
           
           DO y=0,NY-1

              yf=y+1
              IF(yf==NY) yf=0
              yb=y-1
              IF(yb==-1) yb=NY-1
               
! Gather nearest neighbors, 1 is forward x, 2 is backward x, 3 is forward y
! 4 is backward y

              nns(1)=s(xf,y)
              nns(2)=s(xb,y)
              nns(3)=s(x,yf)
              nns(4)=s(x,yb)

! Preselect a new value for the spin


              seed=IAND(seed_m*seed+seed_a,mask)
              news=(q-1)*TWONEG48*seed+1
              news=MOD(s(x,y)+news,q)

! Calculate the number of excited bonds after the change less the
! number of excited bonds before the change

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
              
           ENDDO

        ENDDO

     ENDDO

     acceptance=acceptance/(NX*NY*nit)

! Measure the energy (only forward neighbors are needed) and magnetization
! This can be done in parallel

     energy=0
     mag=0

     energy=energy+COUNT(s/=CSHIFT(s(:,:),1,1))
     energy=energy+COUNT(s/=CSHIFT(s(:,:),1,2))

     DO i=0,q-1

        mag(i)=COUNT(s(:,:)==i) 

     ENDDO

     energy=energy/(2*NX*NY)
     mag=mag/(NX*NY)

     PRINT '("At measurement ",I4,"   energy= ",F8.6,"  acceptance= ",&
          &F8.6)',meas,energy,acceptance
     PRINT '("Magnetizations:")'
     PRINT '(7F10.6)',mag
     PRINT *

  ENDDO

END PROGRAM potts
