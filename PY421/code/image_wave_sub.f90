! This subroutine solves the one-dimensional wave equation.
! After a few blurring process, the lines of the image are taken
! as initial data for the solution of the wave equation.
! wave is called by the graphics driver.

!                          Copyright by Claudio Rebbi, February 2012
 
SUBROUTINE wave(xw,yw,pxa)

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, SAVE :: nstp,step,nstp2
  REAL(REAL8), SAVE :: r 
  REAL(REAL8), DIMENSION(:,:), ALLOCATABLE, SAVE ::  f,fa,p
  INTEGER :: xw,yw,x,y
  INTEGER, DIMENSION(250000) ::  pxa
  REAL(REAL8),SAVE :: dx,dt,dta 
  LOGICAL, SAVE :: initialized=.FALSE.
  
  IF(.NOT.initialized) THEN
     
     OPEN(1,FILE='picture.pxa')
     READ(1,*) xw,yw
     ALLOCATE(f(0:xw-1,0:yw-1),fa(0:xw-1,0:yw-1),p(0:xw-1,0:yw-1))
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
     WRITE(*, '("Enter the total number of preliminary blurring steps")')   
     WRITE(*,'("and the blurring parameter:   ")',ADVANCE='NO')
     READ *,nstp,r
     
  ENDIF
     
  IF(step>0.AND.step<=nstp) THEN
        
     fa=(CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2))/4
     f=(1-r)*f+r*fa
     
  ENDIF
  
  IF(step==nstp) THEN
     
     PRINT *
     WRITE(*,'("Enter the lattice spacing, the time interval, and the")')
     WRITE(*,'("total number of evolution steps: ")', ADVANCE='NO')
     READ *,dx,dt,nstp2
     PRINT *
     
     ! calculate the momentum of the field (equal to df(x-t)/dt = -df/dx 
     ! at t=0)
     
     p=-(CSHIFT(f,1,1)-CSHIFT(f,-1,1))/(2*dx)
      p=0
     
     ! evolution with the leapfrog algorithm: evolve p to t=0.5*dt         
     
     dta=0.5*dt
     p=p+dta*(CSHIFT(f,1,1)+CSHIFT(f,-1,1)-2*f)/dx**2
     p=p+dta*(CSHIFT(f,1,2)+CSHIFT(f,-1,2)-2*f)/dx**2
     dta=dt
     
  END IF

  IF(step>nstp) THEN
        
     ! evolve f from t to t+dt
     
     f=f+dta*p
     
     ! evolve p from t+0.5*dt to t+1.5*dt, but for the last step, where
     ! p is evolved to the final t
     
     p=p+dta*(CSHIFT(f,1,1)+CSHIFT(f,-1,1)-2*f)/dx**2
     p=p+dta*(CSHIFT(f,1,2)+CSHIFT(f,-1,2)-2*f)/dx**2     

  END IF
  
  IF(step==nstp+nstp2) THEN
     
     dta=0.5*dt
     p=p+dta*(CSHIFT(f,1,1)+CSHIFT(f,-1,1)-2*f)/dx**2
     p=p+dta*(CSHIFT(f,1,2)+CSHIFT(f,-1,2)-2*f)/dx**2
     dta=0
     
     PRINT '("Close the window to stop the program.")'
     PRINT *
     
  END IF
  
  step=step+1
  
  CALL field2pxa(pxa,f,xw,yw)
  
END SUBROUTINE wave

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
