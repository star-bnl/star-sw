
      SUBROUTINE PHO_SCALES(Xm1,Xm2,Xm3,Xm4,Scg1,Scg2,Scb1,Scb2)
C**********************************************************************
C
C     calculation of scale factors
C              (mass dependent couplings and slopes)
C
C     input:   XM1..XM4     external masses
C
C     output:  SCG1,SCG2    scales of coupling constants
C              SCB1,SCB2    scales of coupling slope parameter
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ecmmin , ecmtp , EPS , Scb1 , Scb2 , Scg1 , 
     &                 Scg2 , Xm1 , Xm2 , Xm3 , Xm4
      SAVE 
 
      PARAMETER (EPS=1.D-3)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  scale factors for couplings
      ecmmin = 2.D0
C     ECMTP = 6.D0
      ecmtp = 1.D0
      IF ( ABS(Xm1-Xm3).LE.EPS ) THEN
         Scg1 = 1.D0
      ELSE IF ( ECMp.LT.ecmtp ) THEN
         Scg1 = PHIsup(1)*LOG(ECMp**2/ecmmin)/LOG(ecmtp**2/ecmmin)
      ELSE
         Scg1 = PHIsup(1)
      END IF
      IF ( ABS(Xm2-Xm4).LE.EPS ) THEN
         Scg2 = 1.D0
      ELSE IF ( ECMp.LT.ecmtp ) THEN
         Scg2 = PHIsup(2)*LOG(ECMp**2/ecmmin)/LOG(ecmtp**2/ecmmin)
      ELSE
         Scg2 = PHIsup(2)
      END IF
C
C  scale factors for slope parameters
      IF ( (ISWmdl(1).LT.2) .OR. (IPAmdl(10).EQ.1) ) THEN
         Scb1 = 1.D0
         Scb2 = 1.D0
      ELSE IF ( ISWmdl(1).EQ.2 ) THEN
C  rational
         Scb1 = 2.D0*PMAssp(1)**2/(Xm1**2+Xm3**2)
         Scb2 = 2.D0*PMAssp(2)**2/(Xm2**2+Xm4**2)
      ELSE IF ( ISWmdl(1).GE.3 ) THEN
C  symmetric gaussian
         Scb1 = VAR*(Xm1-Xm3)**2
         IF ( Scb1.LT.25.D0 ) THEN
            Scb1 = EXP(-Scb1)
         ELSE
            Scb1 = 0.D0
         END IF
         Scb2 = VAR*(Xm2-Xm4)**2
         IF ( Scb2.LT.25.D0 ) THEN
            Scb2 = EXP(-Scb2)
         ELSE
            Scb2 = 0.D0
         END IF
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I4)')
     &         'PHO_SCALES:ERROR:invalid ISWMDL(1)' , ISWmdl(1)
         CALL PHO_ABORT
      END IF
C  debug output
      IF ( IDEb(65).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E11.3)')
     &         'PHO_SCALES: M1..M4 ' , Xm1 , Xm2 , Xm3 , Xm4
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,4E11.3)')
     &         'SCB1,SCB2,SCG1,SCG2' , Scb1 , Scb2 , Scg1 , Scg2
      END IF
      END SUBROUTINE
