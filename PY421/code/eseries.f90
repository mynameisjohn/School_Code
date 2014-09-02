PROGRAM eseries

  IMPLICIT NONE
  INTEGER :: n, k
  REAL(8) :: x, ee, e=1, y=1

  WRITE(*,'("Enter x and the maximum degree in the expansion: ")',&
       ADVANCE='NO')
  READ *,x,n

  ee=EXP(x);
  PRINT '("Exact value of exp(x): ",F14.9)',ee

  y=1
  e=1
  DO k=1,n
     y=y*x
     y=y/k
     e=e+y    
  END DO

  PRINT '("Power series value &
       &of exp(x): ",F14.9)',e

END PROGRAM eseries
