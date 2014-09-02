! This program implements the solution of the Laplace equation over
! a two dimensional domain with Dirichlet boundary conditions, using
! the Gauss_Jacobi relaxation procedure.  The values of the field are
! fixe over the regions where the logical array inside, as returned 
! by the subroutine init, is set to .FALSE.

!            Copyright by Claudio Rebbi, February 1996, March 2012
 
SUBROUTINE relax(xw,yw,pxa)

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER :: xw,yw,i,x,y
  INTEGER, SAVE :: size,nimg,nstp,npr,img,step,npointsin
  REAL(REAL8), DIMENSION(:,:), ALLOCATABLE, SAVE :: v,vn
  LOGICAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: inside
  REAL(REAL8) :: r 
  INTEGER, DIMENSION(250000) ::  pxa
  REAL(REAL8) :: s,s2 
  LOGICAL, SAVE :: initialized=.FALSE.

  IF(.NOT.initialized) THEN

     WRITE(*,'("Enter the size (<500): ")',ADVANCE='NO')     
     READ *,size 
     PRINT *
     ALLOCATE(v(0:size-1,0:size-1),vn(0:size-1,0:size-1))
     ALLOCATE(inside(0:size-1,0:size-1))
     xw=size
     yw=size
 
     OPEN(1,FILE='relax.data')
  
     CALL init(v,inside,size)
  
     npointsin=COUNT(inside)

     img=0
     step=0
     nimg=10

     vn=(CSHIFT(v,1,1)+CSHIFT(v,-1,1)+CSHIFT(v,1,2)+CSHIFT(v,-1,2))/4
     r=SUM((v-vn)**2,MASK=inside)/npointsin

     PRINT *  
     PRINT '("Total number of steps:",I8,"  normalized residue:",F16.8)',step,r
     WRITE(1,'(I10,F16.6)') step,LOG(r)/2
     PRINT *

     initialized=.TRUE.
     RETURN

  END IF

  IF(img==1) THEN

     WRITE(*,'("Enter the number of images, the number of relaxation steps&
          & per image")')
     WRITE(*,'("and the number of images per print: ")',ADVANCE='NO')     
     READ *,nimg,nstp,npr
     PRINT *

  ENDIF

  IF(img>0.AND.img<=nimg) THEN

     ! Insert here the instructions that perform nstp steps of the 
     ! Gauss-Seidel relaxation algorithm.  After each step increase
     ! the variable step by 1.

  ENDIF

  CALL v2pxa(pxa,v,inside,size)  
 
  IF(img>0.AND.MOD(img,npr)==0.AND.img<=nimg) THEN

     ! Insert here the instructions that calulate the residue,
     ! print the number of steps (the variable step) and the
     ! residue, and write step and 1/2 of the log of the residue
     ! to the file relax.data
     ! (See the initialization part of the program.)

  ENDIF
     
  IF(img==nimg) THEN

     CLOSE(1)

     PRINT *
     PRINT '("Close the window to stop the program.")'
     PRINT *
     
  END IF

  img=img+1

END SUBROUTINE relax

SUBROUTINE init(v0,inside,size)

! This subroutine receives the square arrays v0 and inside as well as
! their size from the calling program and sets the values of v0 equal
! to given potential values over the regions of a size*size square
! which correspond to a given set of conductors.  The other values 
! of v0 are set to 0.  The values of the logical array inside are
! set to .TRUE. in the domain where the potential must be calculated
! and are set to .FALSE. in the regions occupied by the conductors.

!                                                Claudio Rebbi
!                                                Boston University, Feb. 1996 

   IMPLICIT NONE
   INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
   REAL(REAL8), PARAMETER :: x0=0.2_REAL8,x1=0.4_REAL8,y0=0.2_REAL8,&
        y1=0.35_REAL8, xc=0.7_REAL8,yc=0.7_REAL8,r=0.1_REAL8
   INTEGER size,x,y
   REAL(REAL8), DIMENSION(0:size-1,0:size-1) :: v0
   LOGICAL, DIMENSION(0:size-1,0:size-1) :: inside
   REAL(REAL8) :: v1,v2,v3,v4,v5,v6,xa,ya

   ! WRITE(*,'("Enter the potential of the circle: ")',ADVANCE='NO')
   ! READ *,v1
   ! WRITE(*,'("Enter the potential of the rectangle: ")',ADVANCE='NO')
   ! READ *,v2
   ! WRITE(*,'("Enter the potential of the top boundary: ")',ADVANCE='NO')
   ! READ *,v3
   ! WRITE(*,'("Enter the potential of the right boundary: ")',ADVANCE='NO')
   ! READ *,v4
   ! WRITE(*,'("Enter the potential of the bottom boundary: ")',ADVANCE='NO')
   ! READ *,v5
   ! WRITE(*,'("Enter the potential of the left boundary: ")',ADVANCE='NO')
   ! READ *,v6

   WRITE(*,'("Potential of the circle = 20")')
   v1=20
   WRITE(*,'("Potential of the rectangle = -20")')
   v2=-20
   WRITE(*,'("Potential of the top boundary = -10")')
   v3=-10
   WRITE(*,'("Potential of the right boundary = 0")')
   v4=0
   WRITE(*,'("Potential of the bottom boundary = 10")')
   v5=10
   WRITE(*,'("Potential of the left boundary = 0")')
   v6=0

   v0=0
   inside=.TRUE.

   DO y=0,size-1
      ya=REAL(y,REAL8)/size
      DO x=0,size-1
         xa=REAL(x,REAL8)/size
         IF(y==0) THEN
            v0(x,y)=v5
            inside(x,y)=.FALSE.
         ENDIF
         IF(y==size-1) THEN
            v0(x,y)=v3
            inside(x,y)=.FALSE.
         ENDIF
         IF(x==0) THEN
            v0(x,y)=v6
            inside(x,y)=.FALSE.
         ENDIF
         IF(x==size-1) THEN
            v0(x,y)=v4
            inside(x,y)=.FALSE.
         ENDIF
         IF(x0<=xa.AND.xa<=x1.AND.y0<=ya.AND.ya<=y1) THEN
            v0(x,y)=v2
            inside(x,y)=.FALSE.
         ENDIF
         IF((xa-xc)**2+(ya-yc)**2<=r**2) THEN
            v0(x,y)=v1
            inside(x,y)=.FALSE.
         ENDIF
      END DO
   END DO

END SUBROUTINE init


SUBROUTINE v2pxa(pxa,v,inside,size)

! This subroutine receives two square arrays v and inside and their size
! and returns a one dimensional array of color pixel values suitable for
! a graphics representation of the values taken by v.  The color values 
! are defined so that as the values of v range from -infinity to +infinity
! the colors span the rainbow from purple to red, with full color saturation.
! Where the logical array inside is .FALSE. the color is black.

!                                                Claudio Rebbi
!                                                Boston University, Feb. 1996 

   IMPLICIT NONE
   INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
   REAL(REAL8), PARAMETER :: PI=3.141592653589793_REAL8
   INTEGER size,x,y,r,g,b
   REAL(REAL8), DIMENSION(0:size-1,0:size-1) :: v
   INTEGER, DIMENSION(0:size*size-1) :: pxa
   LOGICAL, DIMENSION(0:size-1,0:size-1) :: inside
   REAL(REAL8) t

   DO y=0,size-1
      DO x=0,size-1
         t=(5*(PI/2-ATAN(v(x,y))))/3
         r=128*(2*COS(t)+1)
         IF(r<0) r=0; IF(r>255) r=255
         g=128*(2*COS(t-(2*PI)/3)+1)
         IF(g<0) g=0; IF(g>255) g=255
         b=128*(2*COS(t+(2*PI)/3)+1)
         IF(b<0) b=0; IF(b>255) b=255
         IF(inside(x,y)) THEN
            pxa(x+size*y)=b+ISHFT(g,8)+ISHFT(r,16)
!            pxa(x+size*y)=ISHFT(pxa(x+size*y),8)
         ELSE
            pxa(x+size*y)=0
         ENDIF
      END DO
   END DO

END SUBROUTINE v2pxa
