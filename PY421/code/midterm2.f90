PROGRAM midterm2

! This program solves the equation

! (2*f(i)-f(i+1)-f(i-1))/a**2 + k(i)*f(i) = b(i)

! where periodic boundary conditions are assumed, either by  
! straightforward Gauss-Jacobi relaxation, or by a two-level
! implementation of the relaxation method,

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, PARAMETER :: N=256
  REAL(REAL8), PARAMETER :: PI=3.1415926535898_REAL8

  REAL(REAL8), DIMENSION(0:N-1) :: f  ! field to be determined
  REAL(REAL8), DIMENSION(0:N-1) :: k  ! position dependent term 
                                      ! in the equation
  REAL(REAL8), DIMENSION(0:N-1) :: b  ! given r.h.s. of the equation
  REAL(REAL8), DIMENSION(0:N-1) :: r  ! residue on the fine lattice 
  REAL(REAL8), DIMENSION(0:N-1) :: e  ! error, or correction, on the 
                                      ! fine lattice
  REAL(REAL8) :: a ! the lattice spacing of the fine lattice
  REAL(REAL8) :: rmax ! maximum residue on the fine lattice


  REAL(REAL8), DIMENSION(0:N/2-1) :: kc  ! position dependent term 
                                         ! projected over the
                                         ! coarse lattice
  REAL(REAL8), DIMENSION(0:N/2-1) :: rc  ! residue on the coarse 
                                         ! lattice 
  REAL(REAL8), DIMENSION(0:N/2-1) :: ec  ! error on the coarse lattice
  REAL(REAL8) :: ac ! the lattice spacing of the fine lattice
  
  INTEGER :: ncycles,nfineit,ncoarseit ! number of cycles, number
                                       ! pf relaxation iterations 
                                       ! on the fine lattice, number 
                                       ! of relaxation iterations on 
                                       ! the coarse lattice
  INTEGER :: cycle,fineit,coarseit ! loop parameters for the above
  INTEGER :: nlevel ! 1 for a single level relaxation, 2 for two 
                    ! level relaxation
  INTEGER :: i

  a=1
  ac=2
  f=0

  ! some arbitrary r.h.s. for the equation
  b=0
  b(N/2)=100
  b(N/2-1)=50
  b(N/2+1)=50

  DO i=0,N-1
     k(i)=1+cos((2*PI*i)/N)/5
  END DO

  ! project k onto the coarse lattice, this only needs 
  ! to be done once
  kc=(k(0:N-1:2)+k(1:N-1:2))/2

  WRITE(*,'("Enter 1 for a single level algorithm, 2 for &
       &a 2 level algorithm, any other integer to exit: ")'&
       ,ADVANCE='NO')
  READ *,nlevel
  IF(nlevel/=1.AND.nlevel/=2) STOP

  ncycles=10
  nfineit=3
  ncoarseit=10

  DO cycle=1,ncycles

     ! perform Gauss-Jacobi relaxations on the fine lattice
     DO fineit=1,nfineit
        f=((CSHIFT(f,1)+CSHIFT(f,-1))/a**2+b)/(2/a**2+k)
     END DO

     IF (nlevel==2) THEN

! Insert here the code for the two level implementation:
! (calculate the residue, project over the coarse lattice,
! (perform Gauss-Jacobi relaxations on the coarse lattice,
! (interpolate to the fine lattice and add to f)

     ENDIF

     ! perform again Gauss-Jacobi relaxations on the fine lattice
     DO fineit=1,nfineit
        f=((CSHIFT(f,1)+CSHIFT(f,-1))/a**2+b)/(2/a**2+k)
     END DO

     ! calculate the residue and print its maximum value
     r=b-(2*f-CSHIFT(f,1)-CSHIFT(f,-1))/a-k*f
     
     rmax=MAXVAL(ABS(r))
     PRINT '("At cycle ",I3," max. residue=",F14.9)',cycle,rmax 

  END DO

END PROGRAM midterm2
