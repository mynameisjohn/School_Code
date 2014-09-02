MODULE quaternions

! This module defines quaternions as a type and the overloaded
! operators which implement quaternions algebra.  It also defines
! two simple procedures to input the quaternion components and
! to print them out.


!                                        Claudio Rebbi
!                                        Boston University, February 1997

  IMPLICIT NONE

  TYPE quaternion
     REAL :: r,i1,i2,i3
  END TYPE quaternion

  INTERFACE OPERATOR(+)
     MODULE PROCEDURE q_plus_q
  END INTERFACE

  INTERFACE OPERATOR(-)
     MODULE PROCEDURE q_minus_q
  END INTERFACE

  INTERFACE OPERATOR(*)
     MODULE PROCEDURE q_times_r, r_times_q, q_times_q
  END INTERFACE

  INTERFACE OPERATOR(/)
     MODULE PROCEDURE q_over_r, r_over_q
  END INTERFACE

  INTERFACE CONJG
     MODULE PROCEDURE conjg_q
  END INTERFACE

CONTAINS

  FUNCTION q_plus_q(a,b)
    TYPE(quaternion), INTENT(IN) :: a,b
    TYPE(quaternion) :: q_plus_q
    q_plus_q%r=a%r+b%r
    q_plus_q%i1=a%i1+b%i1
    q_plus_q%i2=a%i2+b%i2
    q_plus_q%i3=a%i3+b%i3
  END FUNCTION q_plus_q

  FUNCTION q_minus_q(a,b)
    TYPE(quaternion), INTENT(IN) :: a,b
    TYPE(quaternion) :: q_minus_q
    q_minus_q%r=a%r-b%r
    q_minus_q%i1=a%i1-b%i1
    q_minus_q%i2=a%i2-b%i2
    q_minus_q%i3=a%i3-b%i3
  END FUNCTION q_minus_q

  FUNCTION q_times_r(a,b)
    TYPE(quaternion), INTENT(IN) :: a
    REAL, INTENT(IN) :: b
    TYPE(quaternion) :: q_times_r
    q_times_r%r=a%r*b
    q_times_r%i1=a%i1*b
    q_times_r%i2=a%i2*b
    q_times_r%i3=a%i3*b
  END FUNCTION q_times_r

  FUNCTION r_times_q(a,b)
    REAL, INTENT(IN) :: a
    TYPE(quaternion), INTENT(IN) :: b
    TYPE(quaternion) :: r_times_q
    r_times_q%r=a*b%r
    r_times_q%i1=a*b%i1
    r_times_q%i2=a*b%i2
    r_times_q%i3=a*b%i3
  END FUNCTION r_times_q

  FUNCTION q_times_q(a,b)
    TYPE(quaternion), INTENT(IN) :: a,b
    TYPE(quaternion) :: q_times_q
    q_times_q%r=a%r*b%r-a%i1*b%i1-a%i2*b%i2-a%i3*b%i3
    q_times_q%i1=a%r*b%i1+a%i1*b%r+a%i2*b%i3-a%i3*b%i2
    q_times_q%i2=a%r*b%i2+a%i2*b%r+a%i3*b%i1-a%i1*b%i3
    q_times_q%i3=a%r*b%i3+a%i3*b%r+a%i1*b%i2-a%i2*b%i1
  END FUNCTION q_times_q

  FUNCTION q_over_r(a,b)
    TYPE(quaternion), INTENT(IN) :: a
    REAL, INTENT(IN) :: b
    TYPE(quaternion) :: q_over_r
    q_over_r%r=a%r/b
    q_over_r%i1=a%i1/b
    q_over_r%i2=a%i2/b
    q_over_r%i3=a%i3/b
  END FUNCTION q_over_r

  FUNCTION r_over_q(a,b)
    REAL, INTENT(IN) :: a
    TYPE(quaternion), INTENT(IN) :: b
    TYPE(quaternion) :: r_over_q
    REAL :: aux
    aux=a/(b%r**2+b%i1**2+b%i2**2+b%i3**2)
    r_over_q%r=aux*b%r
    r_over_q%i1=-aux*b%i1
    r_over_q%i2=-aux*b%i2
    r_over_q%i3=-aux*b%i3
  END FUNCTION r_over_q

  FUNCTION conjg_q(a)
    TYPE(quaternion), INTENT(IN) :: a
    TYPE(quaternion) :: conjg_q
    conjg_q%r=a%r
    conjg_q%i1=-a%i1
    conjg_q%i2=-a%i2
    conjg_q%i3=-a%i3
  END FUNCTION conjg_q

  FUNCTION input_q()
    TYPE(quaternion) :: input_q
    WRITE(*,'("enter the 4 quaternion components: ")',ADVANCE='NO')
    READ *,input_q%r,input_q%i1,input_q%i2,input_q%i3
  END FUNCTION input_q

  SUBROUTINE print_q(a)
    TYPE(quaternion), INTENT(IN) :: a
    WRITE(*,'(F8.3," +",F8.3," i1 +",F8.3," i2 +",F8.3," i3")')&
         a%r,a%i1,a%i2,a%i3
  END SUBROUTINE print_q

END MODULE quaternions
