
      SUBROUTINE DT_SIGVEL(Xi,Q2i,Ecmi,Xnui,Idxv,Svel,Sig1,Sig2)
 
C***********************************************************************
C Cross section for elastic vector meson production                    *
C This version dated 10.05.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ALPHEM , amv , amv2 , bslope , coupl , DT_SIGVP , 
     &                 ecm , Ecmi , GEV2MB , ONE , PI , q2 , Q2i , 
     &                 rosh , selvp , Sig1 , Sig2 , stovp , Svel , 
     &                 TINY10
      DOUBLE PRECISION TWO , TWOPI , w2 , x , Xi , Xnui , ZERO
      INTEGER Idxv
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0,ALPHEM=ONE/137.0D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
 
      w2 = Ecmi**2
      IF ( (Ecmi.LE.ZERO) .AND. (Xnui.GT.ZERO) ) w2 = AAM(1)**2 - Q2i + 
     &     TWO*Xnui*AAM(1)
      q2 = Q2i
      x = Xi
C photoprod.
      IF ( (x.LE.ZERO) .AND. (q2.LE.ZERO) .AND. (w2.GT.ZERO) ) THEN
         q2 = 0.0001D0
         x = q2/(w2+q2-AAM(1)**2)
C DIS
      ELSE IF ( (x.LE.ZERO) .AND. (q2.GT.ZERO) .AND. (w2.GT.ZERO) ) THEN
         x = q2/(w2+q2-AAM(1)**2)
      ELSE IF ( (x.GT.ZERO) .AND. (q2.LE.ZERO) .AND. (w2.GT.ZERO) ) THEN
         q2 = (w2-AAM(1)**2)*x/(ONE-x)
      ELSE IF ( (x.GT.ZERO) .AND. (q2.GT.ZERO) ) THEN
         w2 = q2*(ONE-x)/x + AAM(1)**2
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'SIGVEL: inconsistent input ' , 
     &        w2 , q2 , x
         STOP
      END IF
      ecm = SQRT(w2)
 
      amv = AAM(Idxv)
      amv2 = amv**2
 
      bslope = 2.0D0*(2.0D0+AAM(32)**2/(amv2+q2)
     &         +0.25D0*LOG(w2/(amv2+q2)))*GEV2MB
      rosh = 0.1D0
      stovp = DT_SIGVP(x,q2)/(amv2+q2+RL2)
      selvp = stovp**2*(ONE+rosh**2)/(8.0D0*TWOPI*bslope)
 
      IF ( Idxv.EQ.33 ) THEN
         coupl = 0.00365D0
      ELSE
         STOP
      END IF
      Sig1 = (amv2/(amv2+q2))**2*(ONE+EPSpol*q2/amv2)
      Sig2 = selvp
      Svel = coupl*(amv2/(amv2+q2))**2*(ONE+EPSpol*q2/amv2)*selvp
 
      END SUBROUTINE
