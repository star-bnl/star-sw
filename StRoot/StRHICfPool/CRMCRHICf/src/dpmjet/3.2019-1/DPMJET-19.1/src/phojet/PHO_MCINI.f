
      SUBROUTINE PHO_MCINI
C********************************************************************
C
C     initialization of MC event generation
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ah , ecmpro , edelta , elmin , etablo , PIMASS , 
     &                 sigmax , sprcdf , sprdd , sprsd1 , sprsd2 , 
     &                 TINY , xmpom , xmt1 , xmt2 , xvt1 , xvt2
      INTEGER i , i0 , ic1 , ic2 , ift1 , ift2 , ip , isigm , j , k
      SAVE 
 
      PARAMETER (PIMASS=0.13D0,TINY=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  cross sections
      INCLUDE 'inc/pocsec'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  interpolation tables for hard cross section and MC selection weights
#ifndef FOR_CORSIKA
      INCLUDE 'inc/pohtab'
#else
      INCLUDE 'inc/pohtab50'
#endif
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  cut probability distribution
#ifndef FOR_CORSIKA
      INCLUDE 'inc/poprob'
#else
      INCLUDE 'inc/poprob50'
#endif
C  energy-interpolation table
      INCLUDE 'inc/potabl'
 
      CHARACTER*15 PHO_PNAME
 
      DATA xmpom/0.766D0/
 
C  initialize fragmentation
      CALL PHO_FRAINI(ISWmdl(6))
 
C  reset interpolation tables
      DO k = 1 , 4
         DO j = 1 , IEETA1
            DO i = 1 , 80
               SIGtab(i,j,k,IDXmpar) = 0.D0
            END DO
            SIGecm(j,k,IDXmpar) = 0.D0
         END DO
      END DO
 
C  max. number of allowed colors (large N expansion)
      ic1 = 0
      ic2 = 10000
      CALL PHO_SELCOL(ic1,ic2,0,0,0,0,-1)
 
C  lower energy limit of initialization
      etablo = PARmdl(19)
      IF ( SQSglobmax.LE.5.D0 ) etablo = MIN(2.5D0,etablo)
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,2F12.1)') 
     &                      'PHO_MCINI: selected energy range (SQRT(S))'
     &                      , etablo , SQSglobmax
      IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,A,F7.3,E15.4)')
     &                         'particle 1 (name,mass,virtuality): ' , 
     &                        PHO_PNAME(IFPap(1),1) , PMAss(1) , 
     &                        PVIrt(1)
      IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,A,F7.3,E15.4)')
     &                         'particle 2 (name,mass,virtuality): ' , 
     &                        PHO_PNAME(IFPap(2),1) , PMAss(2) , 
     &                        PVIrt(2)
 
C  cuts on probabilities of multiple interactions
      IMAx = MIN(IPAmdl(32),IIMAX)
      KMAx = MIN(IPAmdl(33),KKMAX)
      ah = 2.D0*PTCut(1)/SQSglobmax
      IMAx = MAX(5,MIN(IMAx,INT(SQSglobmax/2.0D0)))
      KMAx = MIN(KMAx,1+INT(0.9*1.D0/ah))
 
C  hard interpolation table
      ECMf(1,IDXmpar) = SQSglobmax
      ECMf(2,IDXmpar) = 0.9D0*ECMf(1,IDXmpar)
      ECMf(3,IDXmpar) = ECMf(2,IDXmpar)
      ECMf(4,IDXmpar) = ECMf(2,IDXmpar)
      DO k = 1 , 4
         IH_ecm_up(k,IDXmpar) = MIN(IPAmdl(30),MAX_TAB_E)
         IF ( ECMf(k,IDXmpar).LT.100.D0 ) IH_ecm_up(k,IDXmpar)
     &        = MIN(IH_ecm_up(k,IDXmpar),15)
         IF ( ECMf(k,IDXmpar).LT.50.D0 ) IH_ecm_up(k,IDXmpar)
     &        = MIN(IH_ecm_up(k,IDXmpar),10)
         IF ( ECMf(k,IDXmpar).LT.10.D0 ) IH_ecm_up(k,IDXmpar)
     &        = MIN(IH_ecm_up(k,IDXmpar),5)
      END DO
 
C  initialization of hard scattering for all channels and cutoffs
      IF ( HSWcut(5).GT.PARmdl(36) ) CALL PHO_HARMCI(-1,ECMf(1,IDXmpar))
      i0 = 4
      IF ( ISWmdl(2).EQ.0 ) i0 = 1
      DO i = i0 , 1 , -1
         CALL PHO_HARMCI(i,ECMf(i,IDXmpar))
      END DO
 
C  dimension of interpolation table of cut probabilities
      ! IEEMAX = INT(7.D0*LOG10(SQSGLOBMAX)) - 1
      IEEmax = MIN(IPAmdl(31),IEETA1)
      IF ( SQSglobmax.LT.100.D0 ) IEEmax = MIN(IEEmax,15)
      IF ( SQSglobmax.LT.50.D0 ) IEEmax = MIN(IEEmax,10)
      IF ( SQSglobmax.LT.10.D0 ) IEEmax = MIN(IEEmax,5)
      ISImax(IDXmpar) = IEEmax
 
C  calculate probability distribution
      i0 = 4
      ift1 = IFPap(1)
      ift2 = IFPap(2)
      xmt1 = PMAss(1)
      xmt2 = PMAss(2)
      xvt1 = PVIrt(1)
      xvt2 = PVIrt(2)
      IF ( ISWmdl(2).EQ.0 ) i0 = 1
      DO ip = i0 , 1 , -1
         ecmpro = ECMf(ip,IDXmpar)*1.001D0
         IF ( ip.EQ.4 ) THEN
            IFPap(1) = 990
            IFPap(2) = 990
            PMAss(1) = xmpom
            PMAss(2) = xmpom
            PVIrt(1) = 0.D0
            PVIrt(2) = 0.D0
         ELSE IF ( ip.EQ.3 ) THEN
            IFPap(1) = ift2
            IFPap(2) = 990
            PMAss(1) = xmt2
            PMAss(2) = xmpom
            PVIrt(1) = xvt2
            PVIrt(2) = 0.D0
         ELSE IF ( ip.EQ.2 ) THEN
            IFPap(1) = ift1
            IFPap(2) = 990
            PMAss(1) = xmt1
            PMAss(2) = xmpom
            PVIrt(1) = xvt1
            PVIrt(2) = 0.D0
         ELSE
            IFPap(1) = ift1
            IFPap(2) = ift2
            PMAss(1) = xmt1
            PMAss(2) = xmt2
            PVIrt(1) = xvt1
            PVIrt(2) = xvt2
         END IF
         IF ( IEEmax.GT.1 ) THEN
            IF ( ip.EQ.1 ) THEN
               elmin = LOG(etablo)
            ELSE
               elmin = LOG(2.5D0)
            END IF
            edelta = (LOG(ecmpro)-elmin)/DBLE(MAX(1,IEEmax-1))
            DO i = 1 , IEEmax
               ecmpro = EXP(elmin+DBLE(i-1)*edelta)
               CALL PHO_PRBDIS(ip,ecmpro,i)
            END DO
         ELSE
            CALL PHO_PRBDIS(ip,ecmpro,1)
         END IF
 
C  debug output of cross section tables
         IF ( ((IDEb(62).GE.0) .AND. (ip.EQ.1)) .OR. (IDEb(62).GE.3) )
     &        THEN
            IF ( (PVIrt(1)+PVIrt(2).LE.0.01D0) .OR. (IDEb(62).NE.0) )
     &           THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3/1X,A,/1X,A)') 
     &     'Table of total cross sections (mb) for particle combination'
     &     , ip , 
     &   ' Ecm    SIGtot  SIGela  SIGine  SIGqel  SIGsd1  SIGsd2  SIGdd'
     &   , 
     &   '-------------------------------------------------------------'
               DO i = 1 , IEEmax
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,8E9.2)')
     &                 SIGecm(i,ip,IDXmpar) , SIGtab(1,i,ip,IDXmpar) , 
     &                 SIGtab(2,i,ip,IDXmpar) , SIGtab(28,i,ip,IDXmpar)
     &                 , SIGtab(3,i,ip,IDXmpar) , 
     &                 SIGtab(30,i,ip,IDXmpar) + SIGtab(32,i,ip,IDXmpar)
     &                 , SIGtab(31,i,ip,IDXmpar)
     &                 + SIGtab(33,i,ip,IDXmpar) , 
     &                 SIGtab(34,i,ip,IDXmpar) + SIGtab(35,i,ip,IDXmpar)
               END DO
            END IF
            IF ( IDEb(62).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3/1X,A,/1X,A)') 
     &       'Table of partial x-sections (mb) for particle combination'
     &       , ip , 
     &  ' Ecm    SIGSD1L SIGSD1H SIGSD2L SIGSD2H SIGDDL  SIGDDH  SIGCDF'
     &  , 
     &  '--------------------------------------------------------------'
               DO i = 1 , IEEmax
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,8E9.2)')
     &                 SIGecm(i,ip,IDXmpar) , SIGtab(30,i,ip,IDXmpar) , 
     &                 SIGtab(32,i,ip,IDXmpar) , SIGtab(31,i,ip,IDXmpar)
     &                 , SIGtab(33,i,ip,IDXmpar) , 
     &                 SIGtab(34,i,ip,IDXmpar) , SIGtab(35,i,ip,IDXmpar)
     &                 , SIGtab(36,i,ip,IDXmpar)
               END DO
            END IF
            IF ( IDEb(62).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3/1X,A,/1X,A)') 
     &    'Table of born graph x-sections (mb) for particle combination'
     &    , ip , 
     &   ' Ecm    SIGSVDM SIGHRES SIGHDIR SIGTR1  SIGTR2  SIGLOO SIGDPO'
     &   , 
     &   '-------------------------------------------------------------'
               DO i = 1 , IEEmax
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,8E9.2)')
     &                 SIGecm(i,ip,IDXmpar) , SIGtab(56,i,ip,IDXmpar)
     &                 + SIGtab(57,i,ip,IDXmpar) , 
     &                 SIGtab(58,i,ip,IDXmpar) , SIGtab(59,i,ip,IDXmpar)
     &                 , SIGtab(60,i,ip,IDXmpar)
     &                 + SIGtab(61,i,ip,IDXmpar) , 
     &                 SIGtab(62,i,ip,IDXmpar) + SIGtab(63,i,ip,IDXmpar)
     &                 , SIGtab(64,i,ip,IDXmpar) , 
     &                 SIGtab(65,i,ip,IDXmpar) + SIGtab(66,i,ip,IDXmpar)
     &                 + SIGtab(67,i,ip,IDXmpar)
     &                 + SIGtab(68,i,ip,IDXmpar)
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3/1X,A,/1X,A)') 
     &    'Table of unitarized x-sections (mb) for particle combination'
     &    , ip , 
     &   ' Ecm    SIGSVDM SIGHVDM  SIGTR1  SIGTR2  SIGLOO SIGDPO  SLOPE'
     &   , 
     &   '-------------------------------------------------------------'
               DO i = 1 , IEEmax
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,8E9.2)')
     &                 SIGecm(i,ip,IDXmpar) , SIGtab(79,i,ip,IDXmpar) , 
     &                 SIGtab(80,i,ip,IDXmpar) , SIGtab(32,i,ip,IDXmpar)
     &                 , SIGtab(33,i,ip,IDXmpar) , 
     &                 SIGtab(35,i,ip,IDXmpar) , SIGtab(36,i,ip,IDXmpar)
     &                 , SIGtab(39,i,ip,IDXmpar)
               END DO
            END IF
            IF ( IDEb(62).GE.1 ) THEN
               IF ( LPRi.GT.4 )
     &               WRITE (LO,'(/1X,A,/1X,A,2I4,/1X,A,/1X,A)') 
     &    'Table of expected average number of cuts in non-diff events:'
     &    , '       for max. number of cuts soft/hard:' , IMAx , KMAx , 
     &    ' Ecm   PTCUT   SIGNDF   POM-S   POM-H   REG-S' , 
     &    '---------------------------------------------'
               DO i = 1 , IEEmax
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,6E10.3)')
     &                 SIGecm(i,ip,IDXmpar) , SIGtab(77,i,ip,IDXmpar) , 
     &                 SIGtab(78,i,ip,IDXmpar) , SIGtab(74,i,ip,IDXmpar)
     &                 , SIGtab(75,i,ip,IDXmpar) , 
     &                 SIGtab(76,i,ip,IDXmpar)
               END DO
               IF ( ip.EQ.1 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,/1X,A)') 
     &   'Table of rapidity gap survival probability (high-mass diff.):'
     &   , ' Ecm    Spro-sd1     Spro-sd2    Spro-dd    Spro-cd' , 
     &   '---------------------------------------------------'
                  DO i = 1 , IEEmax
                     IF ( SIGecm(i,ip,IDXmpar).GT.10.D0 ) THEN
                        sprsd1 = SIGtab(32,i,ip,IDXmpar)
     &                     /(SIGtab(60,i,ip,IDXmpar)
     &                     -2.D0*(SIGtab(65,i,ip,IDXmpar)
     &                     +SIGtab(66,i,ip,IDXmpar)))
                        sprsd2 = SIGtab(33,i,ip,IDXmpar)
     &                     /(SIGtab(62,i,ip,IDXmpar)
     &                     -2.D0*(SIGtab(65,i,ip,IDXmpar)
     &                     +SIGtab(67,i,ip,IDXmpar)))
                        sprdd = SIGtab(35,i,ip,IDXmpar)
     &                     /(SIGtab(64,i,ip,IDXmpar)
     &                     +SIGtab(61,i,ip,IDXmpar)
     &                     +SIGtab(63,i,ip,IDXmpar)
     &                     -2.D0*(SIGtab(66,i,ip,IDXmpar)
     &                     +SIGtab(67,i,ip,IDXmpar)
     &                     +2.D0*SIGtab(68,i,ip,IDXmpar)))
                        IF ( SIGtab(65,i,ip,IDXmpar).GT.0.D0 ) THEN
                           sprcdf = SIGtab(36,i,ip,IDXmpar)
     &                        /(SIGtab(65,i,ip,IDXmpar)
     &                        +SIGtab(66,i,ip,IDXmpar)
     &                        +SIGtab(67,i,ip,IDXmpar)
     &                        +SIGtab(68,i,ip,IDXmpar))
                        ELSE
                           sprcdf = 0.D0
                        END IF
                        IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,5E10.3)')
     &                       SIGecm(i,ip,IDXmpar) , sprsd1 , sprsd2 , 
     &                       sprdd , sprcdf
                     END IF
                  END DO
               END IF
            END IF
         END IF
      END DO
 
C  simulate only hard scatterings
      IF ( ISWmdl(2).EQ.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'WARNING: generation of hard scatterings only!' , 
     &        '============================================='
         DO i = 2 , 7
            IPRon(i,1) = 0
         END DO
         DO k = 2 , 4
            DO i = 1 , 15
               IPRon(i,k) = 0
            END DO
         END DO
         SIGgen(4) = 0.D0
         DO i = 1 , IEEmax
            sigmax = 0.D0
            IF ( IPRon(1,1).EQ.1 ) sigmax = SIGtab(58,i,1,IDXmpar)
            IF ( IPRon(8,1).EQ.1 ) sigmax = sigmax + 
     &           SIGtab(59,i,1,IDXmpar)
            IF ( sigmax.GT.SIGgen(4) ) THEN
               isigm = i
               SIGgen(4) = sigmax
            END IF
         END DO
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'activated processes, cross section' , 
     &        '----------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         '  nondiffr. resolved processes' , (IPRon(1,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         '            elastic scattering' , (IPRon(2,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         'qelast. vectormeson production' , (IPRon(3,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         '      double pomeron processes' , (IPRon(4,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         ' single diffract. particle (1)' , (IPRon(5,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         ' single diffract. particle (2)' , (IPRon(6,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         '    double diffract. processes' , (IPRon(7,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3,2X,3I3)')
     &         '       direct photon processes' , (IPRon(8,k),k=1,4)
 
C  calculate effective cross section
         SIGgen(4) = 0.D0
         DO i = 1 , IEEmax
            CALL PHO_CSINT(1,IFPap(1),IFPap(2),-1,-1,SIGecm(i,1,IDXmpar)
     &                     ,PVIrt(1),PVIrt(2))
            sigmax = 0.D0
            IF ( ISWmdl(2).GE.1 ) THEN
               IF ( IPRon(1,1).EQ.1 ) sigmax = SIGtot - SIGela - 
     &              SIGvm(0,0) - SIGcdf(0) - SIGlsd(1) - SIGhsd(1)
     &              - SIGlsd(2) - SIGhsd(2) - SIGldd - SIGhdd - SIGdir
               IF ( IPRon(2,1).EQ.1 ) sigmax = sigmax + SIGela
               IF ( IPRon(3,1).EQ.1 ) sigmax = sigmax + SIGvm(0,0)
               IF ( IPRon(4,1).EQ.1 ) sigmax = sigmax + SIGcdf(0)
               IF ( IPRon(5,1).EQ.1 ) sigmax = sigmax + SIGlsd(1)
     &              + SIGhsd(1)
               IF ( IPRon(6,1).EQ.1 ) sigmax = sigmax + SIGlsd(2)
     &              + SIGhsd(2)
               IF ( IPRon(7,1).EQ.1 ) sigmax = sigmax + SIGldd + SIGhdd
               IF ( IPRon(8,1).EQ.1 ) sigmax = sigmax + SIGdir
            ELSE
               IF ( IPRon(1,1).EQ.1 ) sigmax = SIGhar
               IF ( IPRon(8,1).EQ.1 ) sigmax = sigmax + SIGdir
            END IF
            IF ( sigmax.GT.SIGgen(4) ) THEN
               isigm = i
               SIGgen(4) = sigmax
            END IF
         END DO
      END IF
 
C  debug output
      IF ( SIGgen(4).LT.1.D-20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A)') 
     &    'PHO_MCINI:ERROR: selected processes have vanishing x-section'
         STOP
      END IF
      IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,1P3E11.4)')
     &                         'maximum search (Elow/Eup/Epeak)' , 
     &                        SIGecm(1,1,IDXmpar) , 
     &                        SIGecm(IEEmax,1,IDXmpar) , 
     &                        SIGecm(isigm,1,IDXmpar)
      IF ( LPRi.GT.4 ) WRITE (LO,'(11X,A,1PE12.4,/)')
     &                         'max. cross section (mb)' , SIGgen(4)
 
      END SUBROUTINE
