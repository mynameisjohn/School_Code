! This subroutine reads the file "picture.pxa" and returns an array
! suitable for displaying the imag.

!                          Copyright by Claudio Rebbi, February 2013
 
SUBROUTINE blur(xw,yw,pxa)

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  REAL(REAL8), DIMENSION(:,:), ALLOCATABLE, SAVE ::  f,fa
  INTEGER :: xw,yw,x,y
  INTEGER, DIMENSION(250000) ::  pxa
  LOGICAL, SAVE :: initialized=.FALSE.

  IF(.NOT.initialized) THEN

     OPEN(1,FILE='picture.pxa')
     READ(1,*) xw,yw
     ALLOCATE(f(0:xw-1,0:yw-1),fa(0:xw-1,0:yw-1))
          DO y=0,yw-1
        READ(1,*) (f(x,y),x=0,xw-1)
     END DO
     CLOSE(1)
     f=f/765.
     initialized=.TRUE.

     PRINT *
     PRINT '("Close the window to stop the program.")'
     PRINT *

  END IF

  CALL field2pxa(pxa,f,xw,yw)

END SUBROUTINE blur

SUBROUTINE field2pxa(pxa,f,xw,yw)
  
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER xw,yw,x,y
  REAL(REAL8), DIMENSION(0:xw-1,0:yw-1) :: f
  INTEGER, DIMENSION(0:xw*yw-1) :: pxa
  REAL(REAL8) fmax,fmin
  INTEGER r,b,g
  REAL(REAL8), PARAMETER :: EPS=0.0001_REAL8

  fmax=MAXVAL(f)
  fmin=MINVAL(f)
  
  DO y=0,yw-1
     DO x=0,xw-1
        r=0
        g=256*(f(x,y)-fmin)/(fmax-fmin+eps)
        b=g
        pxa(x+xw*(yw-y-1))=r+ISHFT(g,8)+ISHFT(b,16)
     END DO
  END DO
  
END SUBROUTINE field2pxa
