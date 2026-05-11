
      SUBROUTINE PHO_EVEINI(Imode,P1,P2,Ip1,Ip2)
C********************************************************************
C
C     prepare /POEVT1/ for new event
C
C     first subroutine called for each event
C
C     input:   P1(4)  particle 1
C              P2(4)  particle 2
C              IMODE  0    general initialization
C                     1    initialization of particles and kinematics
C                     2    initialization after internal rejection
C
C     output:  IP1,IP2  index of interacting particles
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , EPS , onem , P1 , P2
      INTEGER i , im , Imode , ip , Ip1 , Ip2 , ipcomb , ipos , itmp
      SAVE 
 
      DIMENSION P1(4) , P2(4)
 
      PARAMETER (EPS=1.D-5,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C**anfe COMMON not needed here
C  energy-interpolation table
C       include 'inc/potabl'
C  cross sections
      INCLUDE 'inc/pocsec'
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
C  standard particle data interface
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DIMENSION im(2)
 
C  reset debug variables
      KSPom = 0
      KHPom = 0
      KSReg = 0
      KHDir = 0
      KSTrg = 0
      KHTrg = 0
      KSLoo = 0
      KHLoo = 0
      KSDpo = 0
      KSOft = 0
      KHArd = 0
C
      IDNodf = 0
      IDIfr1 = 0
      IDIfr2 = 0
      IDDpom = 0
      ISTr = 0
      IPOix1 = 0
      IF ( ISWmdl(14).GT.0 ) IPOix1 = 1
      IPOix2 = 0
      IPOix3 = 0
C  reset /POEVT1/ and /POEVT2/
      CALL PHO_REGPAR(0,0,0,0,0,0.D0,0.D0,0.D0,0.D0,0,0,0,0,ipos,0)
      CALL PHO_SELCOL(0,0,0,0,0,0,0)
      DO i = 0 , 10
         IPOwgc(i) = 0
      END DO
 
C  initialization of particle kinematics
C  Initialize elastic scattering with initial kinematics
      IF ( ISWmdl(13).EQ.0 ) THEN
         ipcomb = 0
         IF ( ABS(IFPap(1)).EQ.2212 .AND. IFPap(1).EQ.IFPap(2) ) THEN
            ipcomb = 1
         ELSE IF ( ABS(IFPap(1)).EQ.2212 .AND. IFPap(1).EQ.-IFPap(2) )
     &             THEN
            ipcomb = 2
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,*)
     &            "PHO_EVEINI: Unsupported particle" , 
     &           "combination for JLL elastic scattering model."
            CALL PHO_ABORT
         END IF
C* FLUKA has a separate version of this model.
C         CALL JLL_SET(ECM**2, IPCOMB, 1)
      END IF
C  lepton-photon/hadron-photon vertex and initial particles
      im(1) = 0
      im(2) = 0
      IF ( (IPAmdl(11).GT.0) .AND. (IDPsrc(1).NE.0) ) THEN
         CALL PHO_REGPAR(1,IDPsrc(1),IDBsrc(1),0,0,PINi(1,1),PINi(2,1),
     &                   PINi(3,1),PINi(4,1),0,0,0,0,im(1),1)
      ELSE
         CALL PHO_REGPAR(1,IFPap(1),IFPab(1),im(1),0,P1(1),P1(2),P1(3),
     &                   P1(4),0,0,0,0,Ip1,1)
      END IF
      IF ( (IPAmdl(12).GT.0) .AND. (IDPsrc(2).NE.0) ) THEN
         CALL PHO_REGPAR(1,IDPsrc(2),IDBsrc(2),0,0,PINi(1,2),PINi(2,2),
     &                   PINi(3,2),PINi(4,2),0,0,0,0,im(2),1)
      ELSE
         CALL PHO_REGPAR(1,IFPap(2),IFPab(2),im(2),0,P2(1),P2(2),P2(3),
     &                   P2(4),0,0,0,0,Ip2,1)
      END IF
      IF ( (IPAmdl(11).GT.0) .AND. (IDPsrc(1).NE.0) ) THEN
         CALL PHO_REGPAR(1,IDPsrc(1),IDBsrc(1),im(1),0,PFIn(1,1),
     &                   PFIn(2,1),PFIn(3,1),PFIn(4,1),0,10,0,0,ipos,1)
         CALL PHO_REGPAR(1,IFPap(1),IFPab(1),im(1),0,P1(1),P1(2),P1(3),
     &                   P1(4),0,0,0,0,Ip1,1)
      END IF
      IF ( (IPAmdl(12).GT.0) .AND. (IDPsrc(2).NE.0) ) THEN
         CALL PHO_REGPAR(1,IDPsrc(2),IDBsrc(2),im(2),0,PFIn(1,2),
     &                   PFIn(2,2),PFIn(3,2),PFIn(4,2),0,10,0,0,ipos,1)
         CALL PHO_REGPAR(1,IFPap(2),IFPab(2),im(2),0,P2(1),P2(2),P2(3),
     &                   P2(4),0,0,0,0,Ip2,1)
      END IF
      NEVhep = KACcep
 
      IF ( Imode.LE.1 ) THEN
C  CMS energy
         ECM = SQRT((P1(4)+P2(4))**2-(P1(1)+P2(1))**2-(P1(2)+P2(2))
     &         **2-(P1(3)+P2(3))**2)
C       CALL PHO_PECMS(1,PMASS(1),PMASS(2),ECM,PCM,EE)
         PMAss(1) = PHEp(5,Ip1)
         PVIrt(1) = 0.D0
         IF ( IFPap(1).EQ.22 ) PVIrt(1) = PMAss(1)**2
         PMAss(2) = PHEp(5,Ip2)
         PVIrt(2) = 0.D0
         IF ( IFPap(2).EQ.22 ) PVIrt(2) = PMAss(2)**2
      END IF
 
C  cross section calculations
 
      IF ( Imode.NE.1 ) THEN
         ip = 1
         CALL PHO_CSINT(ip,IFPap(1),IFPap(2),IGHel(1),IGHel(2),ECM,
     &                  PVIrt(1),PVIrt(2))
      END IF
 
      IF ( Imode.LE.0 ) THEN
C  effective cross section
         SIGgen(3) = 0.D0
         IF ( ISWmdl(2).GE.1 ) THEN
            IF ( IPRon(1,1).EQ.1 ) SIGgen(3) = SIGtot - SIGela - 
     &           SIGvm(0,0) - SIGcdf(0) - SIGlsd(1) - SIGhsd(1)
     &           - SIGlsd(2) - SIGhsd(2) - SIGldd - SIGhdd - SIGdir
            IF ( IPRon(2,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGela
            IF ( IPRon(3,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGvm(0,0)
            IF ( IPRon(4,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGcdf(0)
            IF ( IPRon(5,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGlsd(1)
     &           + SIGhsd(1)
            IF ( IPRon(6,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGlsd(2)
     &           + SIGhsd(2)
            IF ( IPRon(7,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGldd + 
     &           SIGhdd
            IF ( IPRon(8,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGdir
C  simulate only hard scatterings
         ELSE
            IF ( IPRon(1,1).EQ.1 ) SIGgen(3) = SIGhar
            IF ( IPRon(8,1).EQ.1 ) SIGgen(3) = SIGgen(3) + SIGdir
         END IF
 
      END IF
 
C  reset of mother/daughter relations only (IMODE = 2)
 
C  debug output
      IF ( IDEb(63).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I12,I3)') 'PHO_EVEINI: ' , 
     &        '/POEVT1/ initialized (event/mode)' , KEVent , Imode
         IF ( Imode.LE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,5X,1P,6E12.4)')
     &            'PHO_EVEINI: ' , 
     &        'current suppression factors total-1/2 hard-1/2 diff-1/2:'
     &        , FSUp , FSUh , FSUd
            onem = -1.D0
            itmp = IDEb(57)
            IDEb(57) = MAX(5,itmp)
            CALL PHO_XSECT(1,0,onem)
            IDEb(57) = itmp
         END IF
         CALL PHO_PREVNT(0)
      END IF
 
      END SUBROUTINE
