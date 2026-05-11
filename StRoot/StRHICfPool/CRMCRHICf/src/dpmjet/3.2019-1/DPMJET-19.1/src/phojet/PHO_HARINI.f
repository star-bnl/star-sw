
      SUBROUTINE PHO_HARINI(Ip,Idp1,Idp2,Pv1,Pv2,Nout,Mode)
C**********************************************************************
C
C     initialize calculation of hard cross section
C
C     must not be called during MC generation
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , dummy , Pv1 , Pv2
      INTEGER Idp1 , Idp2 , Ip , Mode , Nout
      SAVE 
 
      PARAMETER (DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  some constants
      INCLUDE 'inc/pocons'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
 
      DOUBLE PRECISION PHO_ALPHAS
 
      CHARACTER*20 rflag
 
C  set local Pomeron c.m. system data
      IDPdg1 = Idp1
      IDPdg2 = Idp2
      PVIrtp(1) = Pv1
      PVIrtp(2) = Pv2
C  initialize PDFs
      CALL PHO_ACTPDF(IDPdg1,1)
      CALL PHO_ACTPDF(IDPdg2,2)
C  initialize alpha_s calculation
      dummy = PHO_ALPHAS(0.D0,-4)
C  initialize scales with defaults
      IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
         IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
            AQQal = PARmdl(83)
            AQQali = PARmdl(86)
            AQQalf = PARmdl(89)
            AQQpd = PARmdl(92)
            NQQal = IPAmdl(83)
            NQQali = IPAmdl(86)
            NQQalf = IPAmdl(89)
            NQQpd = IPAmdl(92)
         ELSE
            AQQal = PARmdl(82)
            AQQali = PARmdl(85)
            AQQalf = PARmdl(88)
            AQQpd = PARmdl(91)
            NQQal = IPAmdl(82)
            NQQali = IPAmdl(85)
            NQQalf = IPAmdl(88)
            NQQpd = IPAmdl(91)
         END IF
      ELSE IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
         AQQal = PARmdl(82)
         AQQali = PARmdl(85)
         AQQalf = PARmdl(88)
         AQQpd = PARmdl(91)
         NQQal = IPAmdl(82)
         NQQali = IPAmdl(85)
         NQQalf = IPAmdl(88)
         NQQpd = IPAmdl(91)
      ELSE
         AQQal = PARmdl(81)
         AQQali = PARmdl(84)
         AQQalf = PARmdl(87)
         AQQpd = PARmdl(90)
         NQQal = IPAmdl(81)
         NQQali = IPAmdl(84)
         NQQalf = IPAmdl(87)
         NQQpd = IPAmdl(90)
      END IF
      IF ( PARmdl(109+Ip).LT.DEPS ) PARmdl(109+Ip) = AQQal
      IF ( PARmdl(113+Ip).LT.DEPS ) PARmdl(113+Ip) = AQQali
      IF ( PARmdl(117+Ip).LT.DEPS ) PARmdl(117+Ip) = AQQalf
      IF ( PARmdl(121+Ip).LT.DEPS ) PARmdl(121+Ip) = AQQpd
      IF ( IPAmdl(64+Ip).LT.0 ) IPAmdl(64+Ip) = NQQal
      IF ( IPAmdl(68+Ip).LT.0 ) IPAmdl(68+Ip) = NQQali
      IF ( IPAmdl(72+Ip).LT.0 ) IPAmdl(72+Ip) = NQQalf
      IF ( IPAmdl(76+Ip).LT.0 ) IPAmdl(76+Ip) = NQQpd
      AQQal = PARmdl(109+Ip)
      AQQali = PARmdl(113+Ip)
      AQQalf = PARmdl(117+Ip)
      AQQpd = PARmdl(121+Ip)
      NQQal = IPAmdl(64+Ip)
      NQQali = IPAmdl(68+Ip)
      NQQalf = IPAmdl(72+Ip)
      NQQpd = IPAmdl(76+Ip)
      PTCut(1) = PARmdl(36)
      PTCut(2) = PARmdl(37)
      PTCut(3) = PARmdl(38)
      PTCut(4) = PARmdl(39)
      PTAno(1) = PARmdl(130)
      PTAno(2) = PARmdl(131)
      PTAno(3) = PARmdl(132)
      PTAno(4) = PARmdl(133)
      rflag = '(energy-independent)'
      IF ( IPAmdl(7).GT.0 ) rflag = '(energy-dependent)'
 
C  write out all settings
      IF ( (IDEb(66).GE.15) .OR. (Mode.GT.0) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,99010) Ip , IDPdg1 , IDPdg2 , 
     &        PTCut(Ip) , rflag , PDFnam(1) , IGRp(1) , ISEt(1) , 
     &        IEXt(1) , PDFnam(2) , IGRp(2) , ISEt(2) , IEXt(2) , 
     &        PDFlam , NF , NQQal , AQQal , NQQpd , AQQpd
99010    FORMAT (/,' PHO_HARINI: hard scattering parameters for IP:',
     &           I3/,5X,'particle 1 / particle 2:',2I8,/,5X,
     &           'min. PT   :',F7.1,2X,A,/,5X,'PDF side 1:',2X,A8,
     &           ' IGRP/ISET/IEXT ',3I4,/,5X,'PDF side 2:',2X,A8,
     &           ' IGRP/ISET/IEXT ',3I4,/,5X,
     &           'LAMBDA1,2 (4 active flavours):',2F8.3,/,5X,
     &           'max. number of active flavours NF  :',I3,/,5X,
     &           'NQQAL/AQQAL/NQQPD/AQQPD:',I5,F8.3,I5,F8.3)
      END IF
 
      END SUBROUTINE
