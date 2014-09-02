PROGRAM mc_integral

! This program uses a Monte Carlo method to approximate the integral
! over dx_1 dx_2 ... dx_N ranging from -PI/2 to PI/2 of
! f(x_1,x_2,...x_N) = (cos(x_1)**2)*(cos(x_2)**2) ...
! *(cos(x_N)**2), where the parameter N, which should be even,
! is set equal to 10.

! The integral is trivial and its exact value is I=(PI/2)**N.

! The program approximates I first by sampling the intervals
! -PI/2 <= x_i <= PI/2 with uniform probability distribution and
! calculating the expectation value of f.  From

!                  <f>= PI**(-N)*I 

! one derives the approximation

!                   I approx. = PI**(N)*<f> 

! Second, the program approximates I by sampling with a biased
! distribution.  The bias is a Gaussian with prob. distribution

!                 p=EXP(-(x/w)**2/2)/(SQRT(2*PI*w**2)

! f is taken to be zero outside -PI/2 <= x_i <= PI/2.
! In this case 

!    I approx. = SQRT((2*PI*w**2))**N*<f/EXP(sum of -x_i**2/2)> 

! In the program, w is set equal to 1/SQRT(2).  The corresponding
! Gaussian, EXP(-x**2), agrees up to terms of order x**4 (excluded)
! with the behavior of the integrand cos(x)**2 near its peak,
! and the biased sampling reduces the error in the approximation
! by a factor ~ 8.

!                                                 Claudio Rebbi
!                                                 Boston University
!                                                 November 2007

  IMPLICIT NONE

  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, PARAMETER :: N=10,NTRIES=5
  REAL(REAL8), PARAMETER :: PI=3.14159265358979324_REAL8  
  REAL(REAL8), DIMENSION(N) :: x,y
  REAL(REAL8) f,w,p,ex,err,ert,a
  INTEGER ns,i,j,k,in
  INTEGER(LONG) seed
  INTEGER(LONG), PARAMETER :: SEED_M=25214903917_LONG, SEED_A=11_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8


  WRITE(*,'("Enter initial seed and total number of samplings: ")',&
       ADVANCE='NO')
  READ *,seed,ns
  PRINT *

  w=PI
  ert=0
  WRITE(*,'("With uniform sampling, ",I3," tries:")')NTRIES
  DO k=1,NTRIES
     f=0
     DO i=1,ns

        DO in=1,N
           seed=IAND(SEED_M*seed+SEED_A,281474976710655_LONG)
           x(in)=TWONEG48*seed
        END DO
 !       CALL RANDOM_NUMBER(x)
        x=w*x-w/2

        p=1
        DO j=1,N
           p=p*COS(x(j))**2
        END DO
        f=f+p
     END DO
     f=PI**(N)*f/ns
     ex=(PI/2)**N
     err=ABS(f-ex)/ex
     ert=ert+err**2
     WRITE(*,'("MC approx.=",F14.6,"  exact=",F14.6,"  rel.err.=",F14.6)')&
       f,ex,err
  END DO
  WRITE(*,'("Average rel.err.=",F14.6)')SQRT(ert/NTRIES)
  PRINT *
  
  w=SQRT(0.5_REAL8)
  ert=0
  WRITE(*,'("With biased sampling, ",I3," tries:")')NTRIES
  DO k=1,NTRIES
     f=0
     DO i=1,ns

        DO in=1,N
           seed=IAND(SEED_M*seed+SEED_A,281474976710655_LONG)
           y(in)=TWONEG48*seed
        END DO
!        CALL RANDOM_NUMBER(y)
        DO in=1,N,2        
           a=w*SQRT(-2._REAL8*LOG(y(in)))
           x(in)=a*COS(2*PI*y(in+1))
           x(in+1)=a*SIN(2*PI*y(in+1))
        END DO

        p=1
        DO j=1,N
           IF(x(j)<-PI/2.OR.x(j)>PI/2) THEN
              p=0
              EXIT
           END IF
           p=p*COS(x(j))**2*EXP((x(j)/w)**2/2)
        END DO
        f=f+p
     END DO
     f=(2*PI*w**2)**(N/2._REAL8)*f/ns
     err=ABS(f-ex)/ex
     ert=ert+err**2
     WRITE(*,'("MC approx.=",F14.6,"  exact=",F14.6,"  rel.err.=",F14.6)')&
          f,ex,err
  END DO
  WRITE(*,'("Average rel.err.=",F14.6)')SQRT(ert/NTRIES)
  PRINT *
  
END PROGRAM mc_integral
