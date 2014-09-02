PROGRAM pseudo_rand

! This program uses a congruential pseudorandom number generator,
! based on 48 bit seeds, to generate pseudorandom numbers r with
! uniform probability distribution between 0 and 1 (0<=r<1)

! The seed is saved and at each call the seed is replaced by a new
! seed according to

!    seed -> seed' = SEED_M*seed + SEED_A mod 2**48

! where SEED_M and SEED_A are two appropriately chosen numbers.
! They are the same as in the Unix set of functions erand48 etc.
! (see man erand48) 

! The pseudorandom number r is then given by r=seed*2.**(-48)

! Starting from the uniformely distributed random numbers,
! derived random number distributions are obtained.

! Data for histograms of the distribution of random numbers are 
! written to the file rand.data*

! 1) saved on rand.data2

! setting x=-LOG(r) produces numbers with probability distribution
! proportional to EXP(-x)

! 2) saved on rand.data3

! setting x=r**2 produces numbers with probability distribution
! proportional to 1/SQRT(x)

! 3) saved on rand.data4

! setting first rho=SQRT(-2.*LOG(r)), theta=TWOPI*r
! and then
!           x=rho*COS(theta), y=rho*SIN(theta)

! produces two numbers with Gaussian probability distribution and
! mean value 1

! 4) saved on rand.data5

! choosing first r with uniform probability between 0 and 1
! and then accepting or rejecting r according to whether 
! a second pseudorandom number r1 with uniform probability
! distribution between 0 and 1 is <= or > SIN(PI*r) produces
! numbers with probability distribution proportional to SIN(PI*r)

!                                                 Claudio Rebbi
!                                                 April 2009

  IMPLICIT NONE

  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  REAL(REAL8), PARAMETER :: PI=3.141592653589793_REAL8, &
       TWOPI=6.283185307179586_REAL8
  INTEGER(LONG) :: seed
  INTEGER, PARAMETER :: NDIV=20
  REAL rnd48,r,x,y,rho,theta,s,r1
  INTEGER i,n,ir
  REAL, DIMENSION(NDIV) :: hist,hist2

  WRITE(*,'("Enter the initial integer seed:  ")',ADVANCE='NO')
  READ *,seed

  n=40000
  hist=0  
  s=0
  DO i=1,n
     r=rnd48(seed)
     x=-LOG(r)
     s=s+x
     ir=(x/5)*NDIV+1
     IF(ir>NDIV) CYCLE
     hist(ir)=hist(ir)+1
  END DO
  s=s/n
  hist=hist/n
  hist=hist*NDIV/5.

  PRINT '("Integrated distribution within the histogram:  ",F9.6)',&
       5.*SUM(hist)/NDIV    
  PRINT '("Mean value:  ",F9.6)',s

  OPEN(1,FILE='rand.data2')

  DO i=1,NDIV
     WRITE(1,'(2F11.6)')5*(i-1)/(REAL(NDIV)),0.
     WRITE(1,'(2F11.6)')5*(i-1)/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')5*i/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')5*i/(REAL(NDIV)),0.
     WRITE(1,*)
  END DO

  CLOSE(1)

  hist=0  
  s=0
  DO i=1,n
     r=rnd48(seed)
     x=r**2
     ir=x*NDIV+1
     hist(ir)=hist(ir)+1
     s=s+x
  END DO
  s=s/n
  hist=hist/n
  hist=hist*NDIV

  PRINT '("Integrated distribution within the histogram:  ",F9.6)',&
       SUM(hist)/NDIV    
  PRINT '("Mean value:  ",F9.6)',s

  OPEN(1,FILE='rand.data3')

  DO i=1,NDIV
     WRITE(1,'(2F11.6)')(i-1)/(REAL(NDIV)),0.
     WRITE(1,'(2F11.6)')(i-1)/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')i/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')i/(REAL(NDIV)),0.
     WRITE(1,*)
  END DO

  CLOSE(1)

  hist=0  
  hist2=0
  s=0
  DO i=1,n
     r=rnd48(seed)
     rho=SQRT(-2*LOG(r))
     r=rnd48(seed)
     theta=TWOPI*r
     x=rho*COS(theta)
     y=rho*SIN(theta)
     s=s+x**2+y**2
     IF(x>0.) THEN
        ir=(x/4)*NDIV+1
        IF(ir>NDIV) CYCLE
        hist(ir)=hist(ir)+1
     ELSE
        ir=(-x/4)*NDIV+1
        IF(ir>NDIV) CYCLE
        hist2(ir)=hist2(ir)+1
     END IF
     IF(y>0.) THEN
        ir=(y/4)*NDIV+1
        IF(ir>NDIV) CYCLE
        hist(ir)=hist(ir)+1
     ELSE
        ir=(-y/4)*NDIV+1
        IF(ir>NDIV) CYCLE
        hist2(ir)=hist2(ir)+1
     END IF
  END DO
  s=s/(2*n)

  hist=hist/(2*n)
  hist2=hist2/(2*n)
  hist=hist*NDIV/4.
  hist2=hist2*NDIV/4.
  PRINT '("Integrated distribution within the histogram:  ",F9.6)',&
       ((SUM(hist)+SUM(hist2))*4.)/NDIV    
  PRINT '("Mean square value:  ",F9.6)',SQRT(s)

  OPEN(1,FILE='rand.data4')

  DO i=1,NDIV
     WRITE(1,'(2F11.6)')4.*(i-1)/(REAL(NDIV)),0.
     WRITE(1,'(2F11.6)')4.*(i-1)/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')4.*i/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')4.*i/(REAL(NDIV)),0.
     WRITE(1,*)
  END DO
  DO i=1,NDIV
     WRITE(1,'(2F11.6)')-4.*(i-1)/(REAL(NDIV)),0.
     WRITE(1,'(2F11.6)')-4.*(i-1)/(REAL(NDIV)),hist2(i)
     WRITE(1,'(2F11.6)')-4.*i/(REAL(NDIV)),hist2(i)
     WRITE(1,'(2F11.6)')-4.*i/(REAL(NDIV)),0.
     WRITE(1,*)
  END DO

  CLOSE(1)

  hist=0  
  s=0
  i=0
  DO 
     r=rnd48(seed)
     r1=rnd48(seed)
     IF(r1<=SIN(PI*r)) THEN
        ir=r*NDIV+1
        hist(ir)=hist(ir)+1
        s=s+r
        i=i+1
        IF(i>n) EXIT
     END IF
  END DO
  s=s/n
  hist=hist/n
  hist=hist*NDIV

  PRINT '("Integrated distribution within the histogram:  ",F9.6)',&
       SUM(hist)/NDIV    
  PRINT '("Mean value:  ",F9.6)',s

  OPEN(1,FILE='rand.data5')

  DO i=1,NDIV
     WRITE(1,'(2F11.6)')(i-1)/(REAL(NDIV)),0.
     WRITE(1,'(2F11.6)')(i-1)/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')i/(REAL(NDIV)),hist(i)
     WRITE(1,'(2F11.6)')i/(REAL(NDIV)),0.
     WRITE(1,*)
  END DO

  CLOSE(1)

END PROGRAM pseudo_rand

FUNCTION rnd48(seed)

  IMPLICIT NONE
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER(LONG), PARAMETER :: SEED_M=25214903917_LONG, SEED_A=11_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8
  REAL rnd48
  INTEGER(LONG) :: seed

  seed=IAND(SEED_M*seed+SEED_A,281474976710655_LONG)
  rnd48=TWONEG48*seed

END FUNCTION rnd48
