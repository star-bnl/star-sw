
      DOUBLE PRECISION FUNCTION DT_XMHMD(Ecm,Ib,Mode)
 
C***********************************************************************
C Diffractive mass in high mass single/double diffractive events.      *
C This version dated 11.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , DT_SAMPEX , Ecm , OHALF , ONE , r , 
     &                 rr , xcolow , xdiff , xdimax , xdimin , xh , ZERO
      INTEGER Ib , kloop , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (OHALF=0.5D0,ONE=1.0D0,ZERO=0.0D0)
 
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
 
C     DATA XCOLOW /0.05D0/
      DATA xcolow/0.15D0/
 
      DT_XMHMD = ZERO
      xh = XPH(2)
      IF ( Mode.EQ.2 ) xh = XTH(2)
 
C minimum Pomeron-x for high-mass diffraction
C (adjusted to get a smooth transition between HM and LM component)
      r = DT_RNDM(xh)
      xdimin = (3.0D0+400.0D0*r**2)/(xh*Ecm**2)
      IF ( Ecm.LE.300.0D0 ) THEN
         rr = (1.0D0-EXP(-((Ecm/140.0D0)**4)))
         xdimin = (3.0D0+400.0D0*(r**2)*rr)/(xh*Ecm**2)
      END IF
C maximum Pomeron-x for high-mass diffraction
C (coherence condition, adjusted to fit to experimental data)
      IF ( Ib.NE.0 ) THEN
C   baryon-diffraction
         xdimax = xcolow*(1.0D0+EXP(-((Ecm/420.0D0)**2)))
      ELSE
C   meson-diffraction
         xdimax = xcolow*(1.0D0+4.0D0*EXP(-((Ecm/420.0D0)**2)))
      END IF
C check boundaries
      IF ( xdimin.GE.xdimax ) xdimin = OHALF*xdimax
 
      kloop = 0
 100  kloop = kloop + 1
C sample Pomeron-x from 1/x-distribution (critical Pomeron)
      IF ( kloop.GT.20 ) RETURN
      xdiff = DT_SAMPEX(xdimin,xdimax)
C corr. diffr. mass
      DT_XMHMD = Ecm*SQRT(xdiff)
      IF ( DT_XMHMD.LT.2.5D0 ) GOTO 100
 
      END FUNCTION
