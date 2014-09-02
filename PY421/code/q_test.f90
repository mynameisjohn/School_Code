PROGRAM q_test

! This is a simple testing program for the module `quaternions'

!                                        Claudio Rebbi
!                                        Boston University, February 1997


  USE quaternions
  IMPLICIT NONE

  REAL :: a
  TYPE(quaternion) :: p,q,r

  WRITE(*,'("Input p, ")',ADVANCE='NO')
  p=input_q()

  PRINT *

  WRITE(*,'("Input q, ")',ADVANCE='NO')
  q=input_q()

  r=p+q
 
  PRINT *
  WRITE(*,'("p+q=      ")',ADVANCE='NO');  CALL print_q(r)

  r=p-q

  PRINT *
  WRITE(*,'("p-q=      ")',ADVANCE='NO');  CALL print_q(r)

  r=p*q

  PRINT *
  WRITE(*,'("p*q=      ")',ADVANCE='NO');  CALL print_q(r)

  r=q*p

  PRINT *
  WRITE(*,'("q*p=      ")',ADVANCE='NO');  CALL print_q(r)

  a=1
  q=a/p

  PRINT *
  WRITE(*,'("p^(-1)=   ")',ADVANCE='NO');  CALL print_q(q)

  r=p*q

  PRINT *
  WRITE(*,'("p*p^(-1)= ")',ADVANCE='NO');  CALL print_q(r)
  PRINT *

END PROGRAM q_test

