PROGRAM shift

  IMPLICIT NONE

  INTEGER, DIMENSION(0:3) :: a=(/0,1,2,3/), b, d, e
  INTEGER, DIMENSION(2) :: c
  LOGICAL, DIMENSION(0:3) :: f

  PRINT *,a
  b=CSHIFT(a,-2)
  PRINT *,b

  c=a(0:1)
  PRINT *,c

  c=a(1:4:2)
  PRINT *,c

  d=a+b
  PRINT *,d
  e=a*b
  PRINT *,e

  f=a<b
  PRINT *,f

END PROGRAM shift
