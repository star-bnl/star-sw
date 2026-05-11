
      SUBROUTINE PHO_SDECY2(Umo,Am1,Am2,Isp)
C**********************************************************************
C
C     isotropic/anisotropic two particle decay in CM system,
C     (transversely/longitudinally polarized boson into two
C     pseudo-scalar mesons)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , am11 , Am2 , am22 , cod12 , DT_RNDM , 
     &                 sid12 , Umo , umo2 , wau
      INTEGER Isp
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  auxiliary data for three particle decay
      INCLUDE 'inc/po3dcy'
 
      umo2 = Umo*Umo
      am11 = Am1*Am1
      am22 = Am2*Am2
      ECM(1) = (umo2+am11-am22)/(2.D0*Umo)
      ECM(2) = Umo - ECM(1)
      wau = ECM(1)*ECM(1) - am11
      IF ( wau.LT.0.D0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,E12.4)')
     &         'PHO_SDECY2:ERROR:too small mass' , wau
         CALL PHO_ABORT
      END IF
      PCM(1) = SQRT(wau)
      PCM(2) = PCM(1)
 
      CALL PHO_SFECFE(SIF(1),COF(1))
      IF ( Isp.EQ.0 ) THEN
C  no polarization
         COD(1) = 2.D0*DT_RNDM(Umo) - 1.D0
      ELSE IF ( Isp.EQ.1 ) THEN
C  transverse polarization
 50      COD(1) = 2.D0*DT_RNDM(am22) - 1.D0
         sid12 = 1.D0 - COD(1)*COD(1)
         IF ( sid12.LT.DT_RNDM(Am1) ) GOTO 50
      ELSE IF ( Isp.EQ.2 ) THEN
C  longitudinal polarization
 100     COD(1) = 2.D0*DT_RNDM(Am2) - 1.D0
         cod12 = COD(1)*COD(1)
         IF ( cod12.LT.DT_RNDM(am11) ) GOTO 100
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I3)')
     &        'PHO_SDECY2:ERROR: ' , 'invalid polarization' , Isp
         CALL PHO_ABORT
      END IF
 
      COD(2) = -COD(1)
      COF(2) = -COF(1)
      SIF(2) = -SIF(1)
 
      END SUBROUTINE
