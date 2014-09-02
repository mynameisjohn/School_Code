SUBROUTINE fftr(a,k,sg)

! This subroutine calculates the Fourier transform of the complex array
! a by the fast Fourier transform algorithm. The transform is made over
! the first n=2**k elements of a and a itself is overwritten with the
! transformed array on return.  sg is an integer which should take
! values 1 for the direct transform or -1 for the inverse transform.

! With n=2**k, the subroutine implements

!  a(i) --> 1/SQRT(n) * sum over j=0 to n-1 of a(j)*EXP(2*sg*PI*IU*i*j/n)

! where IU stands for the imaginary unit and i ranges from 0 to n-1.

!                                                Claudio Rebbi
!                                                January 1997


  IMPLICIT NONE
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,307)
  COMPLEX(REAL8), PARAMETER :: IU=(0._REAL8,1._REAL8)

  INTEGER k,sg
  COMPLEX(REAL8) a(0:2**k-1)
  COMPLEX(REAL8) aux,ec,fc
  REAL(REAL8) norm
  INTEGER n,i,j,ka,m1,m2,m

! Rearrange the elements of a in bit reversed order and normalize:

  n=ISHFT(1,k)
  norm=1/SQRT(REAL(n,REAL8))

  DO i=0,n-1
     m1=1
     j=0
     DO ka=1,k
        m2=IAND(i,m1)   ! m2 is the bit of i in position ka and it is
        j=IOR(j,ISHFT(m2,k-2*ka+1))  ! put into j in position k-ka+1
        m1=ISHFT(m1,1)
     END DO
     IF (j==i) THEN     ! If the two indices i and j are different
        a(i)=a(i)*norm  ! the elements a(i) and a(j) are swapped
     ELSE IF (j>i) THEN ! All components of the array are also normalized
        aux=a(i)*norm   ! by dividing them by SQRT(n)
        a(i)=a(j)*norm
        a(j)=aux
     ENDIF
  END DO

! Implement the FFT:

  m=1  ! m will take values 1,2,4,8 ... n/2

30 CONTINUE

  DO i=0,m-1

     ! The array elements between 0 and m-1 and those between m and 2*m-1,
     ! which contain Fourier transforms over blocks of size m, must be 
     ! combined with the array elements between m and 2*m-1 to form 
     ! the Fourier transforms over blocks of size 2*m
     ! The same operation must be repeated for the array elements
     ! between 2*m and 3*m-1 and those between 3*m and 4*m-1 etc.
     ! the index j ranges over 0, 2*m, 4*m ... (beginning of the sets of
     ! two consecutive blocks), while the index i ranges over 0 ... m-1,
     ! i.e. inside the blocks
     ! The operation to be performed, for all sets of two consecutive
     ! blocks, is a->a' where
     ! a'(j+ia) = a(j+i)+EXP(IU*PI*ia/m)*a(j+m+i), ia=0 ...2*m-1
     ! and i=(ia mod m)
     ! This is broken into
     ! a'(j+i) = a(j+i)+EXP(IU*PI*i/m)*a(j+m+i)
     ! a'(j+i+m) = a(j+i)-EXP(IU*PI*i/m)*a(j+m+i)
     ! with i=0 ... m-1
     ! for i=0 EXP(IU*PI*i/m)=1
     ! for i>0 EXP(IU*PI*i/m) is equal to the variable fc

     IF (i==0) THEN

        DO j=0,n-1,2*m   
           aux=a(j+m)
           a(j+m)=a(j)-aux
           a(j)=a(j)+aux
        END DO

     ELSE

        DO j=0,n-1,2*m
           aux=a(i+j+m)*fc
           a(i+j+m)=a(i+j)-aux
           a(i+j)=a(i+j)+aux
        END DO

        fc=fc*ec
     ENDIF

  END DO

  m=2*m
  IF(m==n) RETURN

! ec equals the 2*m primitive root of 1, or its conjugate if sg=-1:
! ec=EXP(sg*IU*PI/m), ec**m=-1, ec**(2*m)=1

  IF(m==2) THEN
     ec=IU*sg
  ELSE
     ec=SQRT(ec)
  ENDIF
  fc=ec

  GOTO 30

END SUBROUTINE fftr
