
      SUBROUTINE PHO_CDIFF(Imoth1,Imoth2,Msoft,Mhard,Imode,Irej)
C**********************************************************************
C
C     preparation of /POEVT1/ for double-pomeron scattering
C
C     input:   IMOTH1/2   index of mother particles in /POEVT1/
C
C              IMODE   1  sampling of pomeron-pomeron scattering
C                     -1  initialization
C                     -2  output of statistics
C
C     output:   MSOFT     number of generated soft strings
C               MHARD     number of generated hard strings
C               IREJ      0  accepted
C                         1  rejected
C                        50  user rejection
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DT_RNDM , dum , EPS , pd , xmass
      INTEGER i , i1 , i2 , idir , igen , Imode , Imoth1 , Imoth2 , ip , 
     &        ipar , ipar1 , ipar2 , ipois1 , ipois2 , ipois3 , ipos , 
     &        iproc , Irej , isam , isth1
      INTEGER isth2 , itry2 , itrym , jda11 , jda12 , jda21 , jda22 , 
     &        jsam , khdirs , khpoms , ksam , kspoms , ksregs , Mhard , 
     &        Msoft , nheps
      SAVE 
 
      PARAMETER (EPS=1.D-10,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  some constants
      INCLUDE 'inc/pocons'
C**anfe COMMON not needed here
C  energy-interpolation table
C      include 'inc/potabl'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DIMENSION pd(4)
 
      IF ( Imode.NE.1 ) RETURN
 
      Irej = 0
      ip = 4
C  select first diffraction
      IF ( DT_RNDM(dum).GT.0.5D0 ) THEN
         ipar1 = 1
         ipar2 = 0
      ELSE
         ipar1 = 0
         ipar2 = 1
      END IF
      itry2 = 0
      itrym = 1000
 
C  save current status
      Msoft = 0
      Mhard = 0
      khpoms = KHPom
      kspoms = KSPom
      ksregs = KSReg
      khdirs = KHDir
      ipois1 = IPOix1
      ipois2 = IPOix2
      ipois3 = IPOix3
      jda11 = JDAhep(1,Imoth1)
      jda21 = JDAhep(2,Imoth1)
      jda12 = JDAhep(1,Imoth2)
      jda22 = JDAhep(2,Imoth2)
      isth1 = ISThep(Imoth1)
      isth2 = ISThep(Imoth2)
      nheps = NHEp
 
C  find mother particle production process
      igen = IPHist(2,Imoth1)
      IF ( igen.EQ.0 ) igen = 4
 
C  main generation loop
 
 100  KSPom = kspoms
      KHPom = khpoms
      KHDir = khdirs
      KSReg = ksregs
      i1 = ipar1
      i2 = ipar2
C  reset mother-daugther relations
      NHEp = nheps
      JDAhep(1,Imoth1) = jda11
      JDAhep(2,Imoth1) = jda21
      JDAhep(1,Imoth2) = jda12
      JDAhep(2,Imoth2) = jda22
      ISThep(Imoth1) = isth1
      ISThep(Imoth2) = isth2
      IPOix1 = ipois1
      IPOix2 = ipois2
      IPOix3 = ipois3
C  rejection counter
      itry2 = itry2 + 1
      IF ( itry2.GT.1 ) THEN
         IFAil(39) = IFAil(39) + 1
         IF ( itry2.GE.itrym ) GOTO 200
      END IF
C  generate two diffractive events
      CALL PHO_DIFDIS(i1,i2,Imoth1,Imoth2,1.D0,2,Msoft,Mhard,Irej)
      IF ( Irej.EQ.0 ) THEN
         CALL PHO_DIFDIS(i2,i1,Imoth1,Imoth2,1.D0,2,Msoft,Mhard,Irej)
         IF ( Irej.EQ.0 ) THEN
C  mass of pomeron-pomeron system
            DO i2 = NHEp , 1 , -1
               IF ( IDHep(i2).EQ.990 ) GOTO 120
            END DO
 120        DO i1 = i2 - 1 , 1 , -1
               IF ( IDHep(i1).EQ.990 ) GOTO 140
            END DO
 140        DO i = 1 , 4
               pd(i) = PHEp(i,i1) + PHEp(i,i2)
            END DO
            xmass = (pd(4)-pd(3))*(pd(4)+pd(3)) - pd(1)**2 - pd(2)**2
            IF ( LPRi.GT.4 .AND. IDEb(59).GE.20 )
     &            WRITE (LO,'(1X,A,2I3,E12.4)')
     &            'PHO_CDIFF: IPOM1,IPOM2,MASS**2' , i1 , i2 , xmass
C  af 27/01/2013 corrected minimal squared diff. mass check
            IF ( xmass.LT.MIN(0.1D0,PARmdl(71)**2) ) GOTO 100
            xmass = SQRT(xmass)
            IF ( xmass.LT.PARmdl(71) ) GOTO 100
 
C  sample pomeron-pomeron interaction process
            CALL PHO_DIFPRO(4,ISWmdl(17),990,990,xmass,0.D0,0.D0,1.D0,
     &                      iproc,isam,jsam,ksam,idir)
 
C  non-diffractive pomeron-pomeron interactions
            IF ( (iproc.EQ.1) .OR. (iproc.EQ.8) ) THEN
 150           IF ( isam+jsam+ksam+idir.EQ.0 ) jsam = 1
C  debug output
               IF ( LPRi.GT.4 .AND. IDEb(59).GE.15 )
     &               WRITE (LO,'(1X,A,/5X,I3,E12.4,4I5)')
     &               'PHO_CDIFF: IP,XMASS,ISAM,JSAM,KSAM,IDIR,' , ip , 
     &              xmass , isam , jsam , ksam , idir
C  store debug information
               IF ( idir.GT.0 ) THEN
                  ipar = 4
               ELSE IF ( ksam.GT.0 ) THEN
                  ipar = 3
               ELSE IF ( isam.GT.0 ) THEN
                  ipar = 2
               ELSE
                  ipar = 1
               END IF
               IDDpom = ipar
               IF ( isam+jsam.GT.0 ) KSDpo = 1
               IF ( ksam+idir.GT.0 ) KHDpo = 1
               KSPom = isam
               KSReg = jsam
               KHPom = ksam
               KHDir = idir
               KSTrg = 0
               KSLoo = 0
C  generate pomeron-pomeron interaction
               CALL PHO_STDPAR(i1,i2,igen,isam,jsam,ksam,idir,Irej)
               IF ( Irej.NE.0 ) THEN
                  IFAil(3) = IFAil(3) + 1
                  IF ( ipar.GT.1 ) THEN
                     IF ( ipar.EQ.3 ) IFAil(9) = IFAil(9) + 1
                     IF ( idir.GT.0 ) THEN
                        IFAil(10) = IFAil(10) + 1
                        idir = 0
                     ELSE IF ( ksam.GT.0 ) THEN
                        ksam = ksam - 1
                     ELSE IF ( isam.GT.0 ) THEN
                        isam = isam - 1
                     END IF
                     GOTO 150
                  ELSE
                     IF ( LPRi.GT.4 .AND. IDEb(59).GE.2 )
     &                     WRITE (LO,'(1X,A,2I3,E11.3)') 
     &                  'PHO_CDIFF: rejection by PHO_STDPAR (I,IPAR,XM)'
     &                  , i , ipar , xmass
                     GOTO 200
                  END IF
               END IF
 
C  diffractive pomeron-pomeron interactions
            ELSE
               IPOix2 = IPOix2 + 1
               IPOres(IPOix2) = iproc
               IPOpos(1,IPOix2) = i1
               IPOpos(2,IPOix2) = i2
               ipar = 10 + iproc
               IDDpom = ipar
            END IF
 
C  update debug information
            KSPom = kspoms + isam
            KSReg = ksregs + jsam
            KHPom = khpoms + ksam
            KHDir = khdirs + idir
C  comment line for central diffraction
            CALL PHO_REGPAR(40,4,ipar,Imoth1,Imoth2,pd(1),pd(2),pd(3),
     &                      pd(4),i1,i2,IDHep(Imoth1),IDHep(Imoth2),
     &                      ipos,1)
            PHEp(5,ipos) = xmass
C  debug output
            IF ( IDEb(59).GE.15 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &               'PHO_CDIFF: output of /POEVT1/' , 
     &              '-----------------------------'
               CALL PHO_PREVNT(0)
            END IF
            RETURN
         END IF
      END IF
 
C  treatment of rejection
 200  Irej = 1
      IFAil(40) = IFAil(40) + 1
      IF ( IDEb(59).GE.3 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_CDIFF: rejection (ITRY,ITRYM)' , itry2 , itrym
         IF ( IDEb(59).GE.10 ) THEN
            CALL PHO_PREVNT(0)
         ELSE
            CALL PHO_PREVNT(-1)
         END IF
      END IF
 
      END SUBROUTINE
