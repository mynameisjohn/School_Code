PROGRAM sample_2d_fftr_usage

! This program uses the subroutine fftr to perform a 2-dimensional
! FFT transform and compares the result with the standard definition
! of the Fourier transform.

! The program must be linked with fftr.o

!                  Claudio Rebbi, Boston University, February 2007

  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  COMPLEX(REAL8), PARAMETER :: IU=(0._REAL8,1._REAL8)
  REAL(REAL8), PARAMETER :: TWOPI=6.283185307179586_REAL8

  INTEGER, PARAMETER :: K=8
  INTEGER, PARAMETER :: SIZE=2**K
  COMPLEX(REAL8), DIMENSION(0:SIZE-1,0:SIZE-1) :: fc,ftr
  INTEGER sg,j1,j2,k1,k2

  fc=0
  fc(20,30)=1

  PRINT *
  WRITE(*,'("Initial     fc(1,2) and fc(20,30):  ",2F10.6,",",2F10.6)')&
       fc(1,2),fc(20,30)
  PRINT *

! calculate the Fourier transform according to its straightforward
! definition, only for the indices 1,2 and 20,30, and save the result 
! in ftr

  ftr=0

  j1=1
  j2=2
  DO k1=0,SIZE-1
     DO k2=0,SIZE-1
        ftr(j1,j2)=ftr(j1,j2)+fc(k1,k2)*EXP(TWOPI*IU*(j1*k1+j2*k2)/SIZE)
     END DO
  END DO

  j1=20
  j2=30
  DO k1=0,SIZE-1
     DO k2=0,SIZE-1
        ftr(j1,j2)=ftr(j1,j2)+fc(k1,k2)*EXP(TWOPI*IU*(j1*k1+j2*k2)/SIZE)
     END DO
  END DO

  ftr=ftr/SIZE

  WRITE(*,'("Fourier tr. ftr(1,2) and ftr(20,30):",2F10.6,",",2F10.6)')&
       ftr(1,2),ftr(20,30)
  PRINT *

! calculate the Fourier trsanform by the FFT algorithm

  sg=1
  DO j2=0,SIZE-1
     CALL fftr(fc(0,j2),K,sg) 
  END DO
  fc=TRANSPOSE(fc)
  DO j2=0,SIZE-1
     CALL fftr(fc(0,j2),K,sg) 
  END DO
  fc=TRANSPOSE(fc)

  WRITE(*,'("FFT transf. fc(1,2) and fc(20,30):  ",2F10.6,",",2F10.6)')&
       fc(1,2),fc(20,30)
  PRINT *

  sg=-1
  DO j2=0,SIZE-1
     CALL fftr(fc(0,j2),K,sg) 
  END DO
  fc=TRANSPOSE(fc)
  DO j2=0,SIZE-1
     CALL fftr(fc(0,j2),K,sg) 
  END DO
  fc=TRANSPOSE(fc)

  WRITE(*,'("Inverse FFT fc(1,2) and fc(20,30):  ",2F10.6,",",2F10.6)')&
       fc(1,2),fc(20,30)
  PRINT *

END PROGRAM sample_2d_fftr_usage
