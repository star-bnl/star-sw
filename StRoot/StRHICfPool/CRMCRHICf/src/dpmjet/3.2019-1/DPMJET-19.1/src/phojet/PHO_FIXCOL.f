
      SUBROUTINE PHO_FIXCOL(E1,E2,Theta,Phi,Nev)
C**********************************************************************
C
C     interface to call PHOJET (fixed energy run) with
C     collider kinematics
C
C     equivalen photon approximation to get photon flux
C
C     input:     NEV     number of events to generate
C                THETA   azimuthal angle (micro radians)
C                PHI     beam crossing angle
C                        (with respect to x, in degrees)
C                E1      energy of particle 1 (+z direction, GeV)
C                E2      energy of particle 2 (-z direction, GeV)
C
C     note: particle types have to be specified before
C           with PHO_SETPAR
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION BOG , E1 , E2 , p1 , p2 , ph , Phi , PHO_PMASS , 
     &                 pmass1 , pmass2 , pp1 , pp2 , sigcur , sigmax , 
     &                 th , Theta , TWOPI
      INTEGER i , irej , isavb1 , isavb2 , isavp1 , isavp2 , itry , 
     &        mode , Nev
      SAVE 
 
      PARAMETER (TWOPI=6.283185307D0,BOG=TWOPI/360.0D0)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DIMENSION p1(4) , p2(4)
 
C  remnant initialization (only needed for DPMJET)
      isavp1 = IFPap(1)
      isavb1 = IFPab(1)
      IF ( IFPap(1).EQ.81 ) THEN
         IFPap(1) = IDEqp(1)
         IFPab(1) = IDEqb(1)
      END IF
      isavp2 = IFPap(2)
      isavb2 = IFPab(2)
      IF ( IFPap(2).EQ.82 ) THEN
         IFPap(2) = IDEqp(2)
         IFPab(2) = IDEqb(2)
      END IF
      pmass1 = PHO_PMASS(IFPab(1),0) - SQRT(PVIrt(1))
      pmass2 = PHO_PMASS(IFPab(2),0) - SQRT(PVIrt(2))
      pp1 = SQRT(E1**2-pmass1**2)
      pp2 = SQRT(E2**2-pmass2**2)
C  beam crossing angle
      th = 1.D-6*Theta/2.D0
      ph = Phi*BOG
      p1(1) = pp1*SIN(th)*COS(ph)
      p1(2) = pp1*SIN(th)*SIN(ph)
      p1(3) = pp1*COS(th)
      p1(4) = E1
      p2(1) = pp2*SIN(th)*COS(ph)
      p2(2) = pp2*SIN(th)*SIN(ph)
      p2(3) = -pp2*COS(th)
      p2(4) = E2
      CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
      IFPap(1) = isavp1
      IFPab(1) = isavb1
      IFPap(2) = isavp2
      IFPab(2) = isavb2
      itry = 0
      CALL PHO_PHIST(-1,sigmax)
      CALL PHO_LHIST(-1,sigmax)
C  test of DPMJET interface (default is IPAMDL(13)=0)
      IF ( IPAmdl(13).GT.0 ) THEN
         mode = IPAmdl(13)
         IPAmdl(13) = 0
      ELSE
         mode = 1
      END IF
C  main generation loop
      DO i = 1 , Nev
 50      itry = itry + 1
         CALL PHO_EVENT(mode,p1,p2,sigcur,irej)
         IF ( irej.NE.0 ) GOTO 50
         CALL PHO_PHIST(1,HSWght(0))
         CALL PHO_LHIST(1,HSWght(0))
      END DO
 
      IF ( Nev.GT.0 ) THEN
         sigmax = sigmax*DBLE(Nev)/DBLE(itry)
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/1X,A,1PE12.3,A,/1X,A)') 
     &       '========================================================='
     &       , ' *****   simulated cross section: ' , sigmax , 
     &       ' mb  *****' , 
     &       '========================================================='
         CALL PHO_EVENT(-2,p1,p2,sigcur,irej)
         CALL PHO_PHIST(-2,sigmax)
         CALL PHO_LHIST(-2,sigmax)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &         'POFCOL: no events simulated' , Nev
      END IF
 
      END SUBROUTINE
