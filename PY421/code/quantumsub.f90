! Subroutines used for the program which simulates a path integral.

!                                             Claudio Rebbi
!                                             Boston University
!                                             December 1992
!                                             rewritten in March 1998

SUBROUTINE mcsetup(tg,xd,fe,fo,tc,ce)

! This subroutine initializes the calculation and calculates
! constant arrays used for graphics.

  USE quantummod
  IMPLICIT NONE
  REAL, DIMENSION(0:N) :: tg
  REAL, DIMENSION(-NHIS:NHIS) :: xd,fe,fo
  REAL, DIMENSION(0:NHALF) :: tc,ce
  REAL(REAL8) ee,eo,fet,fot
  REAL, SAVE :: hbarin,lambdain,beta,win
  INTEGER i
  
  DATA hbarin,lambdain,beta,win/1.0,1.0,12.0,0.3/

  PRINT *
  WRITE(*,'("Enter hbar (>=0.2): ")',ADVANCE='NO')
  hbarin=input(hbarin)
  hbar=hbarin
  WRITE(*,'("Enter the coupling constant: ")',ADVANCE='NO')
  lambdain=input(lambdain)
  lambda=lambdain
  WRITE(*,'("Enter the inverse temperature: ")',ADVANCE='NO')
  beta=input(beta)
  t=beta*hbar
  WRITE(*,'("Enter the width of the upgrade: ")',ADVANCE='NO')
  win=input(win)
  w=win
  PRINT *

  dt=t/N
  dti=1/dt

  seed(1)=37
  seed(2)=9123
  seed(3)=43

  nstp=10

  ncorr=0

  DO i=0,N
     tg(i)=0.1+(0.8*i)/N
     x(i)=0
  END DO
  x(-1)=0

  DO i=-NHIS, NHIS
     xd(i)=0.5+(0.2*(i+NHIS))/NHIS
     fdc(i)=0
  END DO

  DO i=0, NHALF
     tc(i)=0.1+(0.35*i)/NHALF
     corrc(i)=0
  END DO

  fdt=0
  fc=0

  CALL exact(fe,ee,fo,eo)
  
  fet=0
  fot=0
  DO i=-NHIS,NHIS
     fet=fet+fe(i)
     fot=fot+fo(i)
  END DO
  DO i=-NHIS,NHIS
     fe(i)=0.05+7*fe(i)/fet
     fo(i)=0.05+7*fo(i)/fot
  END DO
  DO i=0,NHALF
     ce(i)=EXP(-(i*eo*t)/N)*EXP(-((N-i)*ee*t)/N)&
          +EXP(-(i*ee*t)/N)*EXP(-((N-i)*eo*t)/N)
  END DO
  cnf=1./ALOG(ce(0)/ce(NHALF))
  DO i=1,NHALF
     ce(i)=0.1+0.4*(ALOG(ce(i)/ce(0))*cnf+1)
  END DO
  ce(0)=0.5

END SUBROUTINE mcsetup

SUBROUTINE mc(xg)

! This subroutine generates trajectories distributed according to the 
! path integral formula by Metropolis Monte Carlo stochastic sampling.
! It also accumulates the histogram of the distribution of the x-coordinate
! and the correlation x(i)*x(i+j), and calculates a rescaled x(i) array
! (xg) used for the graphic representation of the trajectory.

  USE quantummod
  IMPLICIT NONE
  REAL, DIMENSION(0:N) :: xg
  INTEGER step,i,j,ij
  REAL(REAL8) dx,xn,ds,r,rand48

  DO step=1, nstp

     DO i=0,N-1

        dx=-2
        DO j=1,4
           dx=dx+rand48(seed)
        END DO
        dx=w*dx
        xn=x(i)+dx
        
        ds=(xn+x(i)-x(i+1)-x(i-1))*dx*dti+(lambda*(xn**2-1)**2 &
             -lambda*(x(i)**2-1)**2)*dt
        r=rand48(seed)
        IF(r.LE.EXP(-ds/hbar)) x(i)=xn
        IF(i.EQ.0) x(N)=x(0)
        IF(i.EQ.N-1) x(-1)=x(N-1)
        
        j=0.5*NHIS*x(i)+0.5+NHIS
        j=j-NHIS
        IF(j.GE.-NHIS.AND.j.LE.NHIS) THEN
           fdc(j)=fdc(j)+1
           fdt=fdt+1
        ENDIF
        
     END DO
     
     IF(step<nstp) CYCLE
     
     DO i=0,N
        xg(i)=0.75+0.1*x(i)
        IF (xg(i).GT.1.) xg(i)=1
        IF (xg(i).LT.0.5) xg(i)=0.5
     END DO

     DO i=0, N-1
        fc=fc+x(i)
        DO j=0, NHALF
           ij=i+j
           IF(ij.GE.N) ij=ij-N
           corrc(j)=corrc(j)+x(i)*x(ij)
        END DO
     END DO

     ncorr=ncorr+1

  END DO

END SUBROUTINE mc 

SUBROUTINE measure(fd,cd)

! This subroutine calculates two arrays used for the graphic
! representation of the distribution of the x coordinates and
! of the correlation function.

  USE quantummod
  IMPLICIT NONE
  REAL, DIMENSION(-NHIS:NHIS) :: fd
  REAL, DIMENSION(0:NHALF) :: cd
  INTEGER i,j,ij
  REAL(REAL8) fav

  DO i=-NHIS, NHIS
     fd(i)=0.05+7*fdc(i)/fdt
  END DO

  fav=fc/(N*ncorr)
  DO i=0,NHALF
     corr(i)=corrc(i)/(N*ncorr)-fav**2
     IF(corr(0).GT.0.) cd(i)=corr(i)/corr(0)
     IF (cd(i).LT.0.00001) cd(i)=0.00001
     cd(i)=0.1+0.4*(ALOG(cd(i))*cnf+1)
     IF(cd(i).LT.0.05) cd(i)=0.05
  END DO

END SUBROUTINE measure

SUBROUTINE mcstart

  USE quantummod
  IMPLICIT NONE
  nstp=10

END SUBROUTINE mcstart

SUBROUTINE mcstop

  USE quantummod
  IMPLICIT NONE
  nstp=0

END SUBROUTINE mcstop

SUBROUTINE exact(fe,ee,fo,eo)

! This subroutine calculates, by the shooting method,  the exact values 
! of the energies and wavefunctions of the two lowest states.  In fe (ground 
! state, even wavef.) and fo (first excited level, odd wavef.) it returns
! the corresponding probability densities.

  USE quantummod
  IMPLICIT NONE
  INTEGER, PARAMETER :: NI=10
  REAL, DIMENSION(-NHIS:NHIS) :: fe,fo
  REAL(REAL8) ee,eo
  REAL(REAL8) xmax,xa,dx,psi0,psi1,psi2,emax,de,f0,f1,f2
  INTEGER i,j

  emax=20
  xmax=4
  dx=xmax/(2*NHIS*NI)

  ee=0
  de=0.01_REAL8

1 CONTINUE
  xa=xmax+dx
  f1=2*(lambda*(xa**2-1)**2-ee)/hbar**2
  psi1=0
  xa=xmax
  f2=2*(lambda*(xa**2-1)**2-ee)/hbar**2
  psi2=EXP(-SQRT(f2))*dx
  DO i=2*NHIS,0,-1
     DO j=1,NI
        f0=f1
        f1=f2
        psi0=psi1
        psi1=psi2
        xa=xa-dx
        f2=2*(lambda*(xa**2-1)**2-ee)/hbar**2
        psi2=(2*(1+5*dx**2*f1/12)*psi1-(1-dx**2*f0/12)*psi0)/ &
             (1-dx**2*f2/12)
     END DO
     IF(i.LE.NHIS) THEN
        fe(i)=psi1**2
        fe(-i)=psi1**2
     ENDIF
  END DO

  IF(psi2.GT.psi0.AND.ee.LT.emax) THEN
     ee=ee+de
     GOTO 1
  ELSE
     IF(de.GT.1.0E-5_REAL8) THEN
        ee=ee-de
        de=de/10
        GOTO 1
     ELSE
        GOTO 2
     ENDIF
  ENDIF
  
2 CONTINUE

  eo=ee
  de=0.01_REAL8
  
3 CONTINUE
  xa=xmax+dx
  f1=2*(lambda*(xa**2-1)**2-eo)/hbar**2
  psi1=0
  xa=xmax
  f2=2*(lambda*(xa**2-1)**2-eo)/hbar**2
  psi2=EXP(-SQRT(f2))*dx
  DO i=2*NHIS,0,-1
     DO j=1,NI
        f0=f1
        f1=f2
        psi0=psi1
        psi1=psi2
        xa=xa-dx
        f2=2*(lambda*(xa**2-1)**2-eo)/hbar**2
        psi2=(2*(1+5*dx**2*f1/12)*psi1-(1-dx**2*f0/12)*psi0)/ &
             (1-dx**2*f2/12)
     END DO
     IF(i.LE.NHIS) THEN
        fo(i)=psi1**2
        fo(-i)=psi1**2
     ENDIF
  END DO

  IF(psi1.GT.0..AND.eo.LT.emax) THEN
     eo=eo+de
     GOTO 3
  ELSE
     IF(de.GT.1.0E-5_REAL8) THEN
        eo=eo-de
        de=de/10
        GOTO 3
     ELSE
        RETURN
     ENDIF
  ENDIF

END SUBROUTINE exact
