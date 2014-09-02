! Module used for the program which simulates a path integral.

!                                             Claudio Rebbi
!                                             Boston University
!                                             March 1998

MODULE quantummod

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, PARAMETER :: SHORT=SELECTED_INT_KIND(4)
!  REAL(REAL8), PARAMETER :: PI=3.141592653589793_REAL8, &
!      TWOPI=6.283185307179586_REAL8

  INTEGER, PARAMETER :: N=50  ! number of intervals on the time axis

  INTEGER, PARAMETER :: NHALF=N/2  ! used for the correlation function

  INTEGER, PARAMETER :: NHIS=20   ! one half the number of intervals used 
                                  ! for the histogram of the wave function
                                   



  REAL(REAL8) hbar,lambda,t,w,dt,dti
  REAL(REAL8), DIMENSION(-1:N) :: x
  REAL(REAL8), DIMENSION(-NHIS:NHIS) :: fdc
  REAL(REAL8), DIMENSION(0:NHALF) :: corr,corrc
  REAL(REAL8) fdt,fc,cnf
  INTEGER(SHORT), DIMENSION(3) :: seed

  INTEGER nstp       ! number of evolution steps per frame of animation
  INTEGER ncorr

                     ! these utility functions prompt the user with a 
                     ! suggested input in square brackets and return
                     ! the value entered by the user or the suggested
                     ! value if the user just hits return
  INTERFACE input
     MODULE PROCEDURE input_real, input_integer, input_string
  END INTERFACE

CONTAINS

  FUNCTION input_real(a)

    IMPLICIT NONE
    REAL a,b,input_real
    CHARACTER(LEN=80) c
    INTEGER i,l0,l
    
    WRITE(c,'(F10.4)') a
    l0=0
    l=0
    DO i=1,80
       IF(c(i:i)==' '.AND.l==l0) l0=l0+1
       IF(c(i:i)==' '.AND.l>l0) EXIT
       l=l+1
    END DO
    WRITE(*,'("[",A,"]: ")',ADVANCE='NO') c(l0+1:l)
    READ (*,'(A)') c
    l=0
    DO i=1,80
       IF(c(i:i)==' ') EXIT
       l=l+1
    END DO
    IF(l>0) THEN
       READ(c,*) b
       input_real=b
    ELSE
       input_real=a
    END IF

  END FUNCTION input_real

  FUNCTION input_integer(a)

    IMPLICIT NONE
    INTEGER a,b,input_integer
    CHARACTER(LEN=80) c
    INTEGER i,l0,l
    
    WRITE(c,*) a
    l0=0
    l=0
    DO i=1,80
       IF(c(i:i)==' '.AND.l==l0) l0=l0+1
       IF(c(i:i)==' '.AND.l>l0) EXIT
       l=l+1
    END DO
    WRITE(*,'("[",A,"]: ")',ADVANCE='NO') c(l0+1:l)
    READ (*,'(A)') c
    l=0
    DO i=1,80
       IF(c(i:i)==' ') EXIT
       l=l+1
    END DO
    IF(l>0) THEN
       READ(c,*) b
       input_integer=b
    ELSE
       input_integer=a
    END IF

  END FUNCTION input_integer

  FUNCTION input_string(a,l)

    IMPLICIT NONE
    CHARACTER(LEN=80) a,b,input_string
    INTEGER i,l,la
    
    la=0
    DO i=1,80
       IF(a(i:i)==' ') EXIT
       la=la+1
    END DO
    WRITE(*,'("[",A,"]: ")',ADVANCE='NO') a(1:la)
    READ (*,'(A)') b
    l=0
    DO i=1,80
       IF(b(i:i)==' ') EXIT
       l=l+1
    END DO
    IF(l>0) THEN
       input_string=b
    ELSE
       input_string=a
       l=la
    END IF

  END FUNCTION input_string

END MODULE quantummod
