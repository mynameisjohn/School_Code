PROGRAM linked_list

! This program allows the user to move up and down a linked list
! of integer numbers, to insert a new item in the list, to remove 
! an item and to swap the current item with the next.

  IMPLICIT NONE

  TYPE item
     INTEGER :: entry
     TYPE(item), POINTER :: next
     TYPE(item), POINTER :: previous
  END TYPE item
  
  TYPE(item), POINTER :: current, auxp
 
  CHARACTER(LEN=1) :: char

  ! build the first list

  ALLOCATE(auxp)
  current=>auxp
  
  current%entry=27
  ALLOCATE(current%next)
  current%next%previous=>current

  current=>current%next

  current%entry=8
  ALLOCATE(current%next)
  current%next%previous=>current

  current=>current%next

  current%entry=14
  current%next=>auxp
  current%next%previous=>current

  current=>current%next

  DO

     PRINT *
     IF(ASSOCIATED(current)) THEN
        WRITE(*,'("Current item:",I6,",   next item:",I6)')&
             current%entry,current%next%entry
        PRINT *
        WRITE(*,'("Enter n to move to the next item in the list,& 
             & p to move to the previous item,"/"i to insert an item&
             & after the current item, r to remove the current item,"&
             &/"s to swap the current item with the next,&
             & or any other character to exit:  ")',ADVANCE='NO')
        READ(*,'(A1)')char
     ELSE
        WRITE(*,'("The list is empty, enter i or q to exit:")')        
        READ(*,'(A1)')char
        IF(char/="i".AND.char/="q") CYCLE
     ENDIF

     SELECT CASE(char)
        
     CASE('n')
        ! Insert here the instruction to move to the next item

     CASE('p')
        ! Insert here the instruction to move to the previous item
        
     CASE('i')

        IF(.NOT.ASSOCIATED(current)) THEN
           ALLOCATE(current)
           WRITE(*,'("Enter the first item:  ")',ADVANCE='NO')
           READ *,current%entry
           current%next=>current
           current%previous=>current
        ELSE
           ! Insert here the instructions needed to add a new item
           ! to the list, insering it immediately after the current
           ! item.  Note that at this point of the program the
           ! list contains at least one item and the pointer
           ! "current" is well defined. i.e. associated to a memory
           ! position, since the case of the empty list has been
           ! taken care of above

        END IF
            
     CASE('r')
           
        IF(ASSOCIATED(current,current%next)) THEN
           DEALLOCATE(current)
           CYCLE
        ENDIF
        ! Insert here the instruction needed to remove the current
        ! item, making the previous item the new current item.
        ! Note that at this point in the code the list contains at
        ! least two items, since the case where the list has
        ! been reduced to a single item, so that pointers current
        ! and current%next point to the same position in memory, i.e.
        ! are associated, has been taken care of above
        
     CASE('s')
        ! Insert here the instructions needed to swap the current
        ! item in the list with the next, making the next item
        ! the new current item
         
     CASE DEFAULT
        
        EXIT
        
     END SELECT
     
  END DO

  ! if the list is not empty deallocate all items in the list
  IF(ASSOCIATED(current)) THEN
     auxp=>current
     DO 
        current=>current%next
        IF(ASSOCIATED(current,auxp)) EXIT
        DEALLOCATE(current%previous)
     END DO
  END IF

END PROGRAM linked_list
