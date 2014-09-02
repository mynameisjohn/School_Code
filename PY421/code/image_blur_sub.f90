! This subroutine implements the blurring of an image, which is
! equivalent to a diffusion process.  blur is called by the graphics
! driver program and performs one blurring step each time it is called,
! up to the maximum requested number of steps.

!                          Copyright by Claudio Rebbi, January 2012
 
SUBROUTINE blur(xw,yw,pxa)

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, SAVE :: nstp,npr,step
  REAL(REAL8), SAVE :: r 
  REAL(REAL8), DIMENSION(:,:), ALLOCATABLE, SAVE ::  f,fa
  INTEGER :: xw,yw,x,y
  INTEGER, DIMENSION(250000) ::  pxa
  REAL(REAL8) :: s,s2 
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

     step=0
     nstp=100

  END IF

  IF(step==1) THEN

     PRINT *
     WRITE(*, '("Enter the total number of blurring steps, the number&
       & of steps per print")')   
     WRITE(*,'("and the blurring parameter:   ")',ADVANCE='NO')
     READ *,nstp,npr,r
     PRINT *

     s=SUM(f)/(xw*yw)
     s2=SUM(f**2)/(xw*yw)-s**2
     PRINT '("At step",I6,",  av. intensity ",F10.6,",  fluctuation ",F10.6)',&
          step-1,s,SQRT(s2)

  ENDIF

  IF(step>0) THEN

     fa=(CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2))/4
     f=(1-r)*f+r*fa

  ENDIF

  CALL field2pxa(pxa,f,xw,yw)

  IF(step>0.AND.MOD(step,npr)==0.AND.step<=nstp) THEN

     s=SUM(f)/(xw*yw)
     s2=SUM(f**2)/(xw*yw)-s**2
     PRINT '("At step",I6,",  av. intensity ",F10.6,",  fluctuation ",F10.6)',&
          step,s,SQRT(s2)
     PRINT *,f(0,0),f(0,1)
     PRINT *,f(1,0),f(1,1)
     
  ENDIF
     
  IF(step==nstp) THEN

     PRINT *
     PRINT '("Close the window to stop the program.")'
     PRINT *

  END IF

  IF(step>nstp) r=0

  step=step+1

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
