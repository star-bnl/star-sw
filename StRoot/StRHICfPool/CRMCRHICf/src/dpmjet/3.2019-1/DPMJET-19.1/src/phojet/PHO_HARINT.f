
      SUBROUTINE PHO_HARINT(Ipp,Ecm,P2v1,P2v2,I1,I2,K1,K2,Mspom)
C**********************************************************************
C
C     interpolate cross sections and weights for hard scattering
C
C     input:  IPP    particle combination (neg. for add. user cuts)
C             ECM    CMS energy (GeV)
C             P2V1/2 particle virtualities (pos., GeV**2)
C             I1     first subprocess to calculate
C             I2     last subprocess to calculate
C                    <-1  only scales and cutoffs calculated
C             K1     first variable to calculate
C             K2     last variable to calculate
C             MSPOM  cross sections to use for pt distribution
C                    0  reggeon
C                    >0 pomeron
C
C             for K1 < 3 the soft pt distribution is also calculated
C
C     output: interpolated values in HWgx, HSig, Hdpt
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DEPS2 , Ecm , fac , fac1 , fac2 , P2v1 , 
     &                 P2v2 , PHO_PTCUT , pta1 , q2cut , xx
      INTEGER i , I1 , I2 , il , ip , Ipp , K1 , K2 , l1 , l2 , m , 
     &        Mspom
      SAVE 
 
      PARAMETER (DEPS=1.D-15,DEPS2=2.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  data needed for soft-pt calculation
      INCLUDE 'inc/point3'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  parameters for DGLAP backward evolution in ISR
      INCLUDE 'inc/podgl1'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  interpolation tables for hard cross section and MC selection weights
#ifndef FOR_CORSIKA
      INCLUDE 'inc/pohtab'
#else
      INCLUDE 'inc/pohtab50'
#endif
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  energy-interpolation table
      INCLUDE 'inc/potabl'
 
      DOUBLE PRECISION xp , pts
      DIMENSION xp(2) , pts(0:2,2)
 
      INTEGER iv
      DIMENSION iv(2)
 
      IF ( LPRi.GT.4 .AND. IDEb(58).GE.25 )
     &      WRITE (LO,'(1X,2A,/,5X,I2,3E12.3,5I4)')
     &      'PHO_HARINT: called with ' , 
     &     'IPP,ECM,P2V1,P2V2,I1,I2,K1,K2,MSPOM' , Ipp , Ecm , P2v1 , 
     &     P2v2 , I1 , I2 , K1 , K2 , Mspom
 
      ip = ABS(Ipp)
      IF ( Ipp.GT.0 ) THEN
C  default minimum bias cutoff
         PTCut(ip) = PHO_PTCUT(Ecm,ip)
      ELSE
C  user defined additional cutoff
         PTCut(ip) = HSWcut(4+ip)
      END IF
      PTWant = PTCut(ip)
 
C  ISR cutoffs
      q2cut = MIN(PTWant**2,PARmdl(125+ip))
      Q2Misr(1) = MAX(P2v1,q2cut)
      Q2Misr(2) = MAX(P2v2,q2cut)
C  cutoff for direct photon contribution to photon PDF
      PTAno(ip) = MIN(PTCut(ip),PARmdl(129+ip))
      pta1 = PTAno(ip)
C  scales for hard scattering
      AQQal = PARmdl(109+ip)
      AQQali = PARmdl(113+ip)
      AQQalf = PARmdl(117+ip)
      AQQpd = PARmdl(121+ip)
      NQQal = IPAmdl(64+ip)
      NQQali = IPAmdl(68+ip)
      NQQalf = IPAmdl(72+ip)
      NQQpd = IPAmdl(76+ip)
      IF ( LPRi.GT.4 .AND. IDEb(58).GE.15 )
     &      WRITE (LO,'(1X,A,4I3,4E15.7)') 'PHO_HARINT: scales:' , 
     &     NQQal , NQQali , NQQalf , NQQpd , AQQal , AQQali , AQQalf , 
     &     AQQpd
 
      IF ( I2.LT.-1 ) RETURN
 
      il = ip
      IF ( Ipp.LT.0 ) il = 0
 
C  double-log interpolation
      IF ( Ecm.LT.2.1D0*PTCut(ip) ) THEN
         DO m = I1 , I2
            HFAc(m,IDXmpar) = 0.D0
            HWGx(m,IDXmpar) = 0.D0
            HSIg(m,IDXmpar) = 0.D0
            HDPt(m,IDXmpar) = 0.D0
         END DO
      ELSE
         i = 1
 50      i = i + 1
         IF ( (Ecm.GT.HECm_tab(i,il,IDXmpar)) .AND. 
     &        (i.LT.IH_ecm_up(il,IDXmpar)) ) GOTO 50
 
         IA = 1
         IB = 1
         fac = LOG(Ecm/HECm_tab(i-1,il,IDXmpar))
     &         /LOG(HECm_tab(i,il,IDXmpar)/HECm_tab(i-1,il,IDXmpar))
         DO m = I1 , I2
C  factor due to phase space integration
            xx = LOG(HFAc_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS)
     &           + fac*LOG((HFAc_tab(m,i,IA,IB,il,IDXmpar)+DEPS)
     &           /(HFAc_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS))
            xx = EXP(xx)
            IF ( xx.LT.DEPS2 ) xx = 0.D0
            HFAc(m,IDXmpar) = xx
C  max. weight
            xx = LOG(HWGx_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS)
     &           + fac*LOG((HWGx_tab(m,i,IA,IB,il,IDXmpar)+DEPS)
     &           /(HWGx_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS))
            xx = EXP(xx)
            IF ( xx.LT.DEPS2 ) xx = 0.D0
            HWGx(m,IDXmpar) = xx*1.2D0
C  hard cross section
            xx = LOG(HSIg_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS)
     &           + fac*LOG((HSIg_tab(m,i,IA,IB,il,IDXmpar)+DEPS)
     &           /(HSIg_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS))
            xx = EXP(xx)
            IF ( xx.LT.DEPS2 ) xx = 0.D0
            HSIg(m,IDXmpar) = xx
C  differential hard cross section
            xx = LOG(HDPt_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS)
     &           + fac*LOG((HDPt_tab(m,i,IA,IB,il,IDXmpar)+DEPS)
     &           /(HDPt_tab(m,i-1,IA,IB,il,IDXmpar)+DEPS))
            xx = EXP(xx)
            IF ( xx.LT.DEPS2 ) xx = 0.D0
            HDPt(m,IDXmpar) = xx
         END DO
      END IF
 
      IF ( (K1.LT.3) .AND. (K2.GE.3) ) THEN
C  cross check
         IF ( (I1.GT.9) .OR. (I2.LT.9) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)') 'PHO_HARINT: ' , 
     &           'hard cross section not calculated ' , I1 , I2
         END IF
         SIGh = HSIg(9,IDXmpar)
         DSIghp = HDPt(9,IDXmpar)
C  load soft cross sections from interpolation table
         IF ( Ecm.LE.SIGecm(1,ip,IDXmpar) ) THEN
            l1 = 1
            l2 = 1
         ELSE IF ( Ecm.LT.SIGecm(ISImax(IDXmpar),ip,IDXmpar) ) THEN
            DO i = 2 , ISImax(IDXmpar)
               IF ( Ecm.LE.SIGecm(i,ip,IDXmpar) ) GOTO 60
            END DO
 60         l1 = i - 1
            l2 = i
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3,1P,2E11.3)')
     &            'PHO_HARINT: energy too high (IP,Ecm,Emax)' , ip , 
     &           Ecm , SIGecm(ISImax(IDXmpar),ip,IDXmpar)
            CALL PHO_PREVNT(-1)
            l1 = ISImax(IDXmpar) - 1
            l2 = ISImax(IDXmpar)
         END IF
         fac2 = 0.D0
         IF ( l1.NE.l2 ) fac2 = LOG(Ecm/SIGecm(l1,ip,IDXmpar))
     &        /LOG(SIGecm(l2,ip,IDXmpar)/SIGecm(l1,ip,IDXmpar))
         fac1 = 1.D0 - fac2
         SIGs = fac2*(SIGtab(56,l2,ip,IDXmpar)+SIGtab(57,l2,ip,IDXmpar))
     &          + 
     &          fac1*(SIGtab(56,l1,ip,IDXmpar)+SIGtab(57,l1,ip,IDXmpar))
 
         FS = FPS(ip)
         FH = FPH(ip)
         CALL PHO_SOFTPT(-1,PTWant,PTWant,xp,iv,pts)
      END IF
 
C300  CONTINUE
 
C  debug output
      IF ( IDEb(58).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10,3I2,2E10.3)')
     &         'PHO_HARINT: weights EV,IP,K1/2,ECM,PTC' , KEVent , ip , 
     &        K1 , K2 , Ecm , PTCut(ip)
         DO m = I1 , I2
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,2I3,1p,4E17.8)') m , 
     &           MH_pro_on(m,ip,IDXmpar) , HFAc(m,IDXmpar) , 
     &           HWGx(m,IDXmpar) , HSIg(m,IDXmpar) , HDPt(m,IDXmpar)
         END DO
      END IF
 
      END SUBROUTINE
