
      DOUBLE PRECISION FUNCTION DT_XMLMD(Ecm)
 
C***********************************************************************
C Diffractive mass in high mass single/double diffractive events.      *
C This version dated 11.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a , amax , amo , amu , DT_RNDM , Ecm , r , sam
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C minimum Pomeron-x for low-mass diffraction
C     AMO = 1.5D0
      amo = 2.0D0
C maximum Pomeron-x for low-mass diffraction
C (adjusted to get a smooth transition between HM and LM component)
      r = DT_RNDM(amo)
      sam = 1.0D0
      IF ( Ecm.LE.300.0D0 ) sam = 1.0D0 - EXP(-((Ecm/200.0D0)**4))
      r = DT_RNDM(amo)*sam
      amax = (1.0D0-sam)*SQRT(0.1D0*Ecm**2) + sam*SQRT(400.0D0)
      amu = r*SQRT(100.0D0) + (1.0D0-r)*amax
 
C selection of diffractive mass
C (adjusted to get a smooth transition between HM and LM component)
      r = DT_RNDM(amu)
      IF ( Ecm.LE.50.0D0 ) THEN
         DT_XMLMD = amo*(amu/amo)**r
      ELSE
         a = 0.7D0
         IF ( Ecm.LE.300.0D0 ) a = 0.7D0*(1.0D0-EXP(-((Ecm/100.0D0)**2))
     &        )
         DT_XMLMD = 1.0D0/((r/(amu**a)+(1.0D0-r)/(amo**a))**(1.0D0/a))
      END IF
 
      END FUNCTION
