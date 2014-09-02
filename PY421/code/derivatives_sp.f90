! Approximate the first and second derivative

PROGRAM derivatives

  IMPLICIT NONE

  REAL  :: x, dx, d1ex, d1f, d1b, d1c, d2ex, d2c

  WRITE(*,'(/"Use ctrl C to stop execution"/)')

  WRITE(*,'("Enter the argument of sin(x): ")',ADVANCE='NO')
  READ *,x
  PRINT *

  DO 

     WRITE(*,'("Enter the interval:  ")',ADVANCE='NO')
     READ *,dx
     PRINT *
     
     d1ex=COS(x);
     d2ex=-SIN(x);
     
     d1f=(SIN(x+dx)-SIN(x))/dx;
     WRITE(*,'("Forward ",F18.12,", exact",F18.12,", error",F18.12)') d1f,d1ex,d1f-d1ex
     d1b=(SIN(x)-SIN(x-dx))/dx
     WRITE(*,'("Backward",F18.12,", exact",F18.12,", error",F18.12)') &
          d1b,d1ex,d1b-d1ex
     d1c=(SIN(x+dx)-SIN(x-dx))/(2*dx);
     WRITE(*,'("Central ",F18.12,", exact",F18.12,", &
          &error",F18.12)') d1c,d1ex,d1c-d1ex
     
     PRINT *
     
     d2c=(SIN(x+dx)+SIN(x-dx)-2*SIN(x))/dx**2
     WRITE(*,'("Second der.",F18.12,", exact",F18.12,", error",F18.12)') &
          d2c,d2ex,d2c-d2ex
     
  END DO

END PROGRAM derivatives
