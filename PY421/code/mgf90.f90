! Program multi_grid

!  Copyright by Claudio Rebbi  -  Boston University  -  February 1996
!  This program may be freely copied and used as long as this notice
!  is retained.


! This program solves the equation

!                             M f = r

! by a multigrid procedure based on a Gauss-Seidel relaxation at each
! level.  The matrix M discretizes the 2-dimensional operator  

!               - (d/dx)^2 - (d/dy)^2 + k(x,y) 

! over a square lattice with size given by a power of 2 and periodic boundary
! conditions.  The matrix M is implicitly defined by 

!  Mf=
    
! (4*f-CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2))/a**2+k*f  

! The k array is set to a constant plus a term with sinusoidal variation
! in x and y and the source term r is set to 0, but for one element s(xs,ys)
! which is set to one.

! Set the parameter VERBOSE to .TRUE. to print out the sequence of levels
! at each cycle.

MODULE global

  IMPLICIT NONE
  
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)

  TYPE lattice   
     INTEGER size
     REAL(REAL8) :: a
     REAL(REAL8), DIMENSION(:,:), POINTER :: f,fn,r,k
  END TYPE lattice
  
  TYPE(lattice), DIMENSION(:), ALLOCATABLE :: grid
  
  TYPE mg_step
     INTEGER nit
     TYPE(mg_step), POINTER :: next_step
  END TYPE mg_step
  
  TYPE(mg_step), POINTER :: first_step, current_step
  
  INTEGER level
  
  LOGICAL, PARAMETER :: VERBOSE=.FALSE.
  
END MODULE global

PROGRAM multi_grid
  
  USE global
  
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: MAXLEVELS=8 
  REAL(REAL8), PARAMETER :: PI=3.141592653589793_REAL8
  INTEGER p,size,xs,ys,i,x,y,ncycles,cycle
  REAL(REAL8) k0,dk,a,res1,res2
  REAL(REAL8), DIMENSION(:,:), POINTER :: f,fn,r,k
  TYPE(mg_step), POINTER :: auxp

  DATA k0,dk,xs,ys/0.01_REAL8,0.01_REAL8,0,0/
  
  ! Allocate the lattices:

  WRITE(*,'("The lattice size must be of the form 2**p.  Enter p:  ")',   &
       ADVANCE='NO')
  READ *,p      
  IF(p>MAXLEVELS) THEN
     WRITE(*,'("That ''s a lot of memory you are asking for:")')
     WRITE(*,'("I am willing to give you p=",I3)') MAXLEVELS
     PRINT *
     p=MAXLEVELS
  ENDIF
  
  ALLOCATE(grid(0:p-1))
  
  size=2**p
  a=1
  DO i=0,p-1
     ALLOCATE(grid(i)%f(0:size-1,0:size-1),grid(i)%fn(0:size-1,0:size-1), &
          grid(i)%r(0:size-1,0:size-1),grid(i)%k(0:size-1,0:size-1))
     grid(i)%size=size
     grid(i)%a=a
     size=size/2
     a=a*2
  END DO
  
  PRINT *
  WRITE(*,'("Lattice size:",I4,"  x",I4,"   at the finest level (=0)")')  &
       grid(0)%size,grid(0)%size
  WRITE(*,'("Coarsest level (2 x 2) =",I3)') p-1
  WRITE(*,'("k0: ",F7.4," dk: ",F7.4," source=1 in ",I3,",",I3)') k0,dk,xs,ys
  PRINT *
  
  size=grid(0)%size
  DO y=0,size-1
     DO x=0,size-1
        grid(0)%k(x,y)=k0+dk*SIN((2*PI*x)/size)*SIN((2*PI*y)/size)
     END DO
  END DO
  
  DO i=1,p-1
     size=grid(i-1)%size
     grid(i)%k=(grid(i-1)%k(0:size-1:2,0:size-1:2)    &
          +grid(i-1)%k(1:size-1:2,0:size-1:2)    &
          +grid(i-1)%k(0:size-1:2,1:size-1:2)    &
          +grid(i-1)%k(1:size-1:2,1:size-1:2))/4
  END DO
  
  grid(0)%r=0
  grid(0)%r(xs,ys)=1
  
! Define the multigrid sequence:
  
  WRITE(*,'("How many multigrid cycles would you like to perform?  ")',   &
       ADVANCE='NO') 
  READ *,ncycles
  PRINT *
  
  WRITE(*,'("For each level enter the number of relaxation iterations to &
       &perform, or enter")')   
  WRITE(*,'("a negative number to go one level coarser, 0 to go one level &
       &finer.")')
  WRITE(*,'("Enter 0 at the finest level(0) to terminate the cycle.")')
  PRINT *
  
  ALLOCATE(first_step)
  current_step=>first_step
  level=0
  
  DO 
     
     WRITE(*,'("At level ",I2,":  ")',ADVANCE='NO') level
     READ *,current_step%nit
     
     IF(level==0.AND.current_step%nit==0) EXIT
     IF(level==p-1.AND.current_step%nit<0) THEN
        WRITE(*,'("Maximum level reached:  enter a number >=0.")')
        CYCLE
     ENDIF
     
     IF(current_step%nit<0) level=level+1
     IF(current_step%nit==0) level=level-1
     
     ALLOCATE(current_step%next_step)
     current_step=>current_step%next_step
     
  END DO
  
  PRINT *
  
  current_step%next_step=>first_step
  
  ! Execute the multigrid cycles:
   
  current_step=>first_step
  
  f=>grid(0)%f
  fn=>grid(0)%fn
  r=>grid(0)%r
  k=>grid(0)%k
  a=grid(0)%a
  size=grid(0)%size
  
  f=0
  
  DO cycle=1,ncycles
     
     WRITE(*,'("Cycle no. ",I3)') cycle
     IF(VERBOSE) PRINT *
     
     CALL mg_cycle
     
     fn=CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2)
     fn=r+(fn-4*f)/a**2-k*f
     res1=SQRT(SUM(fn*fn))/size
     res2=MAXVAL(ABS(fn))     
     
     IF(VERBOSE) PRINT *   
     WRITE(*,'("Av. sq. res. =",F12.8,"  max. res. =",F12.8)')res1,res2
     PRINT * 
     
  END DO
   
  ! Deallocate memory

  auxp=>current_step

  DO 
     
     current_step=>current_step%next_step
     DEALLOCATE(auxp)
     auxp=>current_step
     IF(ASSOCIATED(auxp,first_step)) EXIT

  END DO

  DO i=0,p-1
     DEALLOCATE(grid(i)%f,grid(i)%fn, grid(i)%r,grid(i)%k)
  END DO

  DEALLOCATE(grid)

END PROGRAM multi_grid

RECURSIVE SUBROUTINE mg_cycle
  
  USE global 
  
  IMPLICIT NONE
  REAL(REAL8), DIMENSION(:,:), POINTER :: f,fn,r,k
  REAL(REAL8) a,aux
  INTEGER x,y,xf,xb,yf,yb,it,size
  
  DO
     
     SELECT CASE(current_step%nit)
        
     CASE(1:)
        
        ! Perform the requested number of relaxation iterations:
        
        IF(VERBOSE) WRITE(*,'("At level ",I2,": ",I3," iterations")') &
             level,current_step%nit
        f=>grid(level)%f
        r=>grid(level)%r
        k=>grid(level)%k
        a=grid(level)%a
        size=grid(level)%size
        
        DO it=1,current_step%nit
           DO y=0,size-1
              yf=IAND(y+1,size-1)
              yb=IAND(y+size-1,size-1)
              DO x=0,size-1
                 xf=IAND(x+1,size-1)
                 xb=IAND(x+size-1,size-1)
                 aux=f(xf,y)+f(xb,y)+f(x,yf)+f(x,yb)
                 f(x,y)=(aux+r(x,y)*a**2)/(4+k(x,y)*a**2)
              END DO
           END DO
        END DO
        
        current_step=>current_step%next_step
        
     CASE(:-1)
        
! Calculate the residue, project on the coarser lattice and go up one
! level.  Call mg_cycle again:
        
        IF(VERBOSE)   &
             WRITE(*,'("At level ",I2,":   go to coarser lattice")') level
        
        f=>grid(level)%f
        fn=>grid(level)%fn
        r=>grid(level)%r
        k=>grid(level)%k
        a=grid(level)%a
        fn=CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2)
        fn=r+(fn-4*f)/a**2-k*f
        
        size=grid(level)%size
        level=level+1
        f=>grid(level)%f
        r=>grid(level)%r
        f=0
        r=(fn(0:size-1:2,0:size-1:2)+fn(1:size-1:2,0:size-1:2) &
             +fn(0:size-1:2,1:size-1:2)+fn(1:size-1:2,1:size-1:2))/4   
        
        current_step=>current_step%next_step
        CALL mg_cycle
        
     CASE(0)
        
! Interpolate the result on a finer lattice, go down one level (if not 
! already on the finest lattice) and return:

        IF(VERBOSE)    &
             WRITE(*,'("At level ",I2,":   go to finer lattice")') level
        
        IF(level>0) THEN
           fn=>grid(level)%f
           level=level-1
           size=grid(level)%size
           f=>grid(level)%f
           f(0:size-1:2,0:size-1:2)=f(0:size-1:2,0:size-1:2)+fn 
           f(1:size-1:2,0:size-1:2)=f(1:size-1:2,0:size-1:2)+fn 
           f(0:size-1:2,1:size-1:2)=f(0:size-1:2,1:size-1:2)+fn 
           f(1:size-1:2,1:size-1:2)=f(1:size-1:2,1:size-1:2)+fn 
        ENDIF
        current_step=>current_step%next_step
        RETURN
        
     END SELECT
     
  END DO
  
END SUBROUTINE mg_cycle
