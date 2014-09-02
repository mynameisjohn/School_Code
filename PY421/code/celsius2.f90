PROGRAM celsius2

! Print a Fahrenheit-Celsius conversion table 

  IMPLICIT NONE  

  IntEgER fahr
  REAL celsius
  INTEGER :: lower=0, upper=300, step=20
  
  fahr=lower
  DO WHILE(fahr<=upper)
     celsius=5.0*(fahr-32)/9;   !semicolon not needed
     PRINT '(I4,F8.2)', fahr, celsius
     fahr=fahr+step
     
  END DO
  
END PROGRAM celsius2

