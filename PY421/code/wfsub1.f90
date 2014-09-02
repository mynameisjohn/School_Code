! Subroutines used for a simplified version of the program
! which solves the time dependent Schroedinger equation
! and displays an animation of the wave function.

!                                             Claudio Rebbi
!                                             Boston University
!                                             March 1998

SUBROUTINE wfsetup(xgr,vgr)

  USE wfmod
  IMPLICIT NONE
  REAL(REAL8) x0,width,p,x1,x2,vh
  REAL(REAL8), DIMENSION(0:NX-1) :: lambda
  REAL, DIMENSION(0:NXG) :: xgr,vgr
  INTEGER i,k
  REAL c1,c2

  hbar=1

  dt=0.00012
  nstp=5

  a=L/NX
  DO i=0,NX-1
     x(i)=-L/2+i*a
  END DO

  x1=3
  x2=5
  vh=4
  v=0
  WHERE(x1<=x.AND.x<=x2) v=vh

  xgr(0:NXG-1)=x(0:NX-1:NXSK)
  xgr(NXG)=L/2
  vgr(0:NXG-1)=v(0:NX-1:NXSK)
  vgr(NXG)=vgr(0)

  expv=EXP(-IU*v*dt/hbar)

  DO k=0,NX-1
     lambda(k)=(2-2*COS((TWOPI*k)/NX))/a**2
  ENDDO
  expk=EXP(-IU*hbar*lambda*dt/2)

  x0=-3
  width=2
  p=3
  psi=EXP(-((x-x0)/width)**2+IU*p*x/hbar)

END SUBROUTINE wfsetup

SUBROUTINE wfevolve(psigr,psic)

  USE wfmod
  IMPLICIT NONE
  REAL, PARAMETER :: PSIMIN=1.E-10
  REAL, DIMENSION(0:NXG) :: psigr,phi
  REAL, DIMENSION(0:2,0:NXG) :: psic
  INTEGER i,job

  DO i=1,nstp
     
     psi=expv*psi
      job=1
      CALL fftr(psi,KX,job)
     psi=expk*psi
      job=-1
      CALL fftr(psi,KX,job)

  END DO

  psigr(0:NXG-1)=PSISF*(REAL(psi(0:NX-1:NXSK))**2+AIMAG(psi(0:NX-1:NXSK))**2)
  psigr(NXG)=psigr(0)

  phi(0:NXG-1)=ATAN2(AIMAG(psi(0:NX-1:NXSK)),REAL(psi(0:NX-1:NXSK),REAL8))
  phi(NXG)=phi(0)
  WHERE(psigr<PSIMIN) phi=0

  psic(0,:)=COS(phi)+0.5
  psic(1,:)=COS(phi+(TWOPI)/3)+0.5
  psic(2,:)=COS(phi-(TWOPI)/3)+0.5
  WHERE(psic>1.) psic=1
  WHERE(psic<0.) psic=0

END SUBROUTINE wfevolve

SUBROUTINE wfevolve1s(psigr,psic)

  USE wfmod
  IMPLICIT NONE
  REAL, PARAMETER :: PSIMIN=1.E-10
  REAL, DIMENSION(0:NXG) :: psigr,phi
  REAL, DIMENSION(0:2,0:NXG) :: psic
  INTEGER i

  DO i=1,nstp
     
     psi=psi-IU*dt*(hbar*(2*psi-CSHIFT(psi,1)-CSHIFT(psi,-1))/(2*a**2) &
          +v*psi/hbar)

  END DO

  psigr(0:NXG-1)=PSISF*(REAL(psi(0:NX-1:NXSK))**2+AIMAG(psi(0:NX-1:NXSK))**2)
  psigr(NXG)=psigr(0)

  phi(0:NXG-1)=ATAN2(AIMAG(psi(0:NX-1:NXSK)),REAL(psi(0:NX-1:NXSK),REAL8))
  phi(NXG)=phi(0)
  WHERE(psigr<PSIMIN) phi=0
  psic(0,:)=COS(phi)+0.5
  psic(1,:)=COS(phi+(TWOPI)/3)+0.5
  psic(2,:)=COS(phi-(TWOPI)/3)+0.5
  WHERE(psic>1.) psic=1
  WHERE(psic<0.) psic=0

END SUBROUTINE wfevolve1s

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

  DO i=0,n
     m1=1
     j=0
     DO ka=1,k
        m2=IAND(i,m1)
        j=IOR(j,ISHFT(m2,k-2*ka+1))
        m1=ISHFT(m1,1)
     END DO
     IF (j==i) THEN
        a(i)=a(i)*norm
     ELSE IF (j>i) THEN
        aux=a(i)*norm
        a(i)=a(j)*norm
        a(j)=aux
     ENDIF
  END DO

! Implement the FFT:

  m=1

30 CONTINUE

  DO i=0,m-1

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

! ec equals the 2*m root of -1:

  IF(m==2) THEN
     ec=IU*sg
  ELSE
     ec=SQRT(ec)
  ENDIF
  fc=ec

  GOTO 30

END SUBROUTINE fftr
