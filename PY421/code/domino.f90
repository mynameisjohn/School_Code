PROGRAM domino

  IMPLICIT NONE
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: N=20
  INTEGER(LONG) :: seed
  LOGICAL, DIMENSION(0:N-1,0:N-1) :: free
  CHARACTER(LEN=1), DIMENSION(0:N-1,0:N-1) :: board
  CHARACTER(LEN=1) :: blank='-',star='*',dum
  REAL :: rnd48
  INTEGER :: meas,nmeas,drop,ndrop,x,y,dir,x2,y2
  REAL :: f

  free=.TRUE.

  PRINT *
  WRITE(*,'("Enter the initial seed:  ")',ADVANCE='NO')
  READ *,seed  
  PRINT *
  WRITE(*,'("Enter the number of progressive occupancy measurements and"/&
       &"the number of simulated drops between measurements:  ")',&
       ADVANCE='NO')
  READ *,nmeas,ndrop
  PRINT *

  DO meas=1,nmeas
     
     DO drop=1,ndrop

        ! Insert here the instructions needed to simulate the
        ! fall of a domino piece on the board.  If any of the
        ! two squares over which the piece falls is occupied
        ! i.e. if the corresponding entry in the array free
        ! is set to .FALSE., just cycle to the next piece.  
        ! Otherwise set the apprpriate elements of the array 
        ! free to .FALSE. and the corresponding characters in 
        ! the array board to star 

     END DO

     f=COUNT(free)/REAL(N*N)
     WRITE(*,'("After",I6,"  drops, the unoccupied fraction is",F9.6)')&
          meas*ndrop,f 

  END DO
  
  WHERE(free)
     board=blank
  ELSEWHERE
     board=star
  END WHERE
  
  PRINT *
  WRITE(*,'("Enter any character to show the final board:  ")',ADVANCE='NO')
  READ *,dum
   
  PRINT *
  DO y=N-1,0,-1
     WRITE(*,'(20A3)') board(:,y)
  END DO
  PRINT *

END PROGRAM domino

FUNCTION rnd48(seed)

  IMPLICIT NONE
  INTEGER, PARAMETER :: LONG=SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER(LONG), PARAMETER :: SEED_M=25214903917_LONG, SEED_A=11_LONG
  REAL(REAL8), PARAMETER :: TWONEG48=0.35527136788005009E-14_REAL8
  REAL rnd48
  INTEGER(LONG) :: seed

  seed=IAND(SEED_M*seed+SEED_A,281474976710655_LONG)
  rnd48=TWONEG48*seed

END FUNCTION rnd48
