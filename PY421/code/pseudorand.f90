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

!                                                 Claudio Rebbi
!                                                 April 2009

  IMPLICIT NONE

  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER(LONG) :: seed
  REAL rnd48, r
  INTEGER i,n

  WRITE(*,'("Enter the initial integer seed:  ")',ADVANCE='NO')
  READ *,seed

  n=20
  
  DO i=1,n
     r=rnd48(seed)
     PRINT '(F9.6,I22)',r,seed
  END DO
    
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
