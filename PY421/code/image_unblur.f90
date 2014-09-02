! This subroutine implements first the blurring of an image, which is
! equivalent to a diffusion process.  blur is called by the graphics
! driver program and performs one blurring step each time it is called,
! up to the requested number of steps.  Then the subroutine unblurs
! the image using the expansion of the image into normal modes of the 
! diffusion (or blurring) operator.

!                          Copyright by Claudio Rebbi, February 2012
 
SUBROUTINE blur(xw,yw,pxa)

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  REAL(REAL8), PARAMETER :: TWOPI=6.283185307179586_8, EPS=1.E-10_8
  INTEGER, PARAMETER :: K=8
  INTEGER, PARAMETER :: SIZE=2**K
  INTEGER, SAVE :: nstp,step,nstp2
  REAL(REAL8), SAVE :: r 
  REAL(REAL8), DIMENSION(SIZE,SIZE), SAVE ::  f,fa
  INTEGER :: xw,yw,x,y
  INTEGER, DIMENSION(0:SIZE*SIZE-1) ::  pxa
  REAL(REAL8), DIMENSION(0:SIZE-1,0:SIZE-1) ::  lambda
  COMPLEX(8), DIMENSION(0:SIZE-1,0:SIZE-1) ::  fc
  REAL(REAL8), DIMENSION(0:SIZE-1) :: cosk
  INTEGER :: job
  LOGICAL, SAVE :: initialized=.FALSE.
  
  IF(.NOT.initialized) THEN
     
     OPEN(1,FILE='picture.pxa')
     READ(1,*) xw,yw
     IF(xw<SIZE.OR.yw<SIZE) THEN
        PRINT *,'Image too small'
        STOP
     END IF
     xw=SIZE
     yw=SIZE
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
     WRITE(*,'("Enter the total number of unblurring steps: ")',ADVANCE='NO')
     READ *,nstp2

     fc=f

! insert here the code that unblurs the image
  
     PRINT *
     PRINT '("Close the window to stop the program.")'
     PRINT *     
  END IF
    
  step=step+1
  
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

SUBROUTINE fftr(a,k,sg)

! This subroutine calculates the Fourier transform of the complex array
! a by the fast Fourier transform algorithm. The transform is made over
! the first n=2**k elements of a and a itself is overwritten with the
! transformed array on return.  sg is an integer which should take
! values 1 for the direct transform or -1 for the inverse transform.

! With n=2**k, the subroutine implements

!  a(i) --> 1/SQRT(n) * sum over j=0 to n-1 of a(j)*EXP(2*sg*PI*IU*i*j/n)

! where IU stands for the imaginary unit and i ranges from 0 to n-1.

!                                                Claudio Rebbi
!                                                January 1997


  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  COMPLEX(REAL8), PARAMETER :: IU=(0._REAL8,1._REAL8)

  INTEGER k,sg
  COMPLEX(REAL8) a(0:2**k-1)
  COMPLEX(REAL8) aux,ec,fc
  REAL(REAL8) norm
  INTEGER n,i,j,ka,m1,m2,m

! Rearrange the elements of a in bit reversed order and normalize:

  n=ISHFT(1,k)
  norm=1/SQRT(REAL(n,REAL8))

  DO i=0,n
     m1=1
     j=0
     DO ka=1,k
        m2=IAND(i,m1)
        j=IOR(j,ISHFT(m2,k-2*ka+1))
        m1=ISHFT(m1,1)
     END DO
     IF (j==i) THEN
        a(i)=a(i)*norm
     ELSE IF (j>i) THEN
        aux=a(i)*norm
        a(i)=a(j)*norm
        a(j)=aux
     ENDIF
  END DO

! Implement the FFT:

  m=1

30 CONTINUE

  DO i=0,m-1

     IF (i==0) THEN

        DO j=0,n-1,2*m
           aux=a(j+m)
           a(j+m)=a(j)-aux
           a(j)=a(j)+aux
        END DO

     ELSE

        DO j=0,n-1,2*m
           aux=a(i+j+m)*fc
           a(i+j+m)=a(i+j)-aux
           a(i+j)=a(i+j)+aux
        END DO

        fc=fc*ec
     ENDIF

  END DO

  m=2*m
  IF(m==n) RETURN

! ec equals the 2*m root of -1:

  IF(m==2) THEN
     ec=IU*sg
  ELSE
     ec=SQRT(ec)
  ENDIF
  fc=ec

  GOTO 30

END SUBROUTINE fftr
