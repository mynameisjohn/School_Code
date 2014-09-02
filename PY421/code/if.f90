PROGRAM if

  ! This program illustrates the use of the IF statement

  IMPLICIT NONE

  INTEGER :: x

  DO

     WRITE(*,'("Enter a number, positive or negative, or 0 to quit: ")',&
          ADVANCE='NO')
     READ *,x

     IF(x>0) THEN
        PRINT *,"The number is positive"
     ELSE IF(x<0) THEN
        PRINT *,"The number is negative"
     ELSE
        EXIT
     END IF

  END DO

END PROGRAM if
