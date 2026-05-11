
      SUBROUTINE PHO_FIXLAB(Plab,Nev)
C**********************************************************************
C
C     interface to call PHOJET (fixed energy run) with
C     LAB kinematics (second particle as target)
C
C     equivalent photon approximation to get photon flux
C
C     input:     NEV     number of events to generate
C                PLAB    LAB momentum of particle 1
C
C     note: particle types have to be specified before
C           with PHO_SETPAR
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , irej , isavb1 , isavb2 , isavp1 , isavp2 , itry , Nev
      DOUBLE PRECISION p1 , p2 , PHO_PMASS , Plab , pmass1 , pmass2 , 
     &                 sigcur , sigmax
      SAVE 
 
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
      SPCm = Plab
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
C  get momenta in LAB system
      pmass1 = PHO_PMASS(IFPab(1),0)**2 - PVIrt(1)
      pmass2 = PHO_PMASS(IFPab(2),0)**2 - PVIrt(2)
      IF ( pmass2.LT.0.1D0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2I7)')
     &        'PHO_FIXLAB:ERROR: ' , 'no LAB system possible' , IFPab(1)
     &        , IFPab(2)
      ELSE
         p1(1) = 0.D0
         p1(2) = 0.D0
         p1(3) = Plab
         p1(4) = SQRT(pmass1+Plab**2)
         p2(1) = 0.D0
         p2(2) = 0.D0
         p2(3) = 0.D0
         p2(4) = SQRT(pmass2)
         CALL PHO_EVENT(-1,p1,p2,sigmax,irej)
         IFPap(1) = isavp1
         IFPab(1) = isavb1
         IFPap(2) = isavp2
         IFPab(2) = isavb2
         itry = 0
         CALL PHO_PHIST(-1,sigmax)
         CALL PHO_LHIST(-1,sigmax)
C  event generation loop
         DO i = 1 , Nev
 20         itry = itry + 1
            CALL PHO_EVENT(1,p1,p2,sigcur,irej)
            IF ( irej.NE.0 ) GOTO 20
            CALL PHO_LHIST(1,HSWght(0))
 
            CALL PHO_PHIST(10,HSWght(0))
 
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
     &            'PHO_FIXLAB: no events simulated' , Nev
         END IF
      END IF
 
      END SUBROUTINE
