PROGRAM sample_fftr_usage

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)

  INTEGER, PARAMETER :: K=10
  INTEGER, PARAMETER :: SIZE=2**K
  COMPLEX(REAL8), DIMENSION(0:SIZE-1) :: f
  INTEGER sg

  f=0
  f(20)=1

  PRINT *,f(1),f(20)
  PRINT *

  sg=1
  CALL fftr(f,K,sg)

  PRINT *,f(1),f(20)
  PRINT *

  sg=-1
  CALL fftr(f,K,sg)

  PRINT *,f(1),f(20)
  PRINT *

END PROGRAM sample_fftr_usage

