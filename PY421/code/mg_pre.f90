PROGRAM mg_pre

  IMPLICIT NONE
  
  INTEGER, PARAMETER :: MAXLEVELS=8
  INTEGER ncycles,cycle,level
  
  TYPE mg_step
     INTEGER nit
     TYPE(mg_step), POINTER :: next_step
  END TYPE mg_step
  
  TYPE(mg_step), POINTER :: first_step, current_step, auxp
  
  
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
     IF(level==MAXLEVELS-1.AND.current_step%nit<0) THEN
        WRITE(*,'("Maximum level reached:  enter a number >=0.")')
        CYCLE
     ENDIF
     
     IF(current_step%nit<0) level=level+1
     IF(current_step%nit==0) level=level-1
     
     ALLOCATE(current_step%next_step)
     current_step=>current_step%next_step
     
  END DO
  
  current_step%next_step=>first_step

  ! Execute the multigrid cycles:
  
  current_step=>first_step
  level=0
  
  DO cycle=1,ncycles
     
     WRITE(*,'("Cycle no. ",I3)') cycle
     PRINT *
     
     DO 
        
        IF(level==0.AND.current_step%nit==0) THEN
           current_step=>current_step%next_step
           EXIT
        ENDIF
        
        IF(current_step%nit>0) THEN
           WRITE(*,'("At level ",I2,": ",I3," iterations")') &
                level,current_step%nit
        ELSE IF(current_step%nit<0) THEN
           WRITE(*,'("At level ",I2,":   go to coarser lattice")') level
           level=level+1
        ELSE
           WRITE(*,'("At level ",I2,":   go to finer lattice")') level
           level=level-1
        ENDIF
        
        current_step=>current_step%next_step
        
     END DO
     
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

END PROGRAM mg_pre
