PROGRAM files

  ! This program illustrates access to files in Fortran

  IMPLICIT NONE
  CHARACTER(LEN=35) :: a
  INTEGER :: b
  REAL :: c

  OPEN(1,FILE='files.data')
  OPEN(21,FILE='files.output')

  READ(1,*) a
  PRINT *,a,"End of the line"
  PRINT '(A50,"End of the line")',a
  READ(1,'(A20)') a
  PRINT *,a,"End of the line"

  DO
     READ(1,*,END=100)b,c
     PRINT *,b,c
     WRITE(21,'(I6,F12.4)')b,c
  END DO

100  CONTINUE

  CLOSE(1)
  CLOSE(21)

END PROGRAM files
