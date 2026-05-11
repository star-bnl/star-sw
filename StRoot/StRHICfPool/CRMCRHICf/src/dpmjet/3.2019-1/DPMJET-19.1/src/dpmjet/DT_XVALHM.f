
      SUBROUTINE DT_XVALHM(Kp,Kt)
 
C***********************************************************************
C Sampling of parton x-values in high-mass diffractive interactions.   *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_DBETAR , DT_RNDM , DT_SAMPEX , OHALF , ONE , 
     &                 TINY2 , unon , xpohi , xpolo , xvqthr , ZERO
      INTEGER iflav , Kp , Kt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0,TINY2=1.0D-2)
 
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      DATA unon , xvqthr/2.0D0 , 0.8D0/
 
      IF ( Kp.EQ.2 ) THEN
C x-fractions of projectile valence partons
 50      XPH(1) = DT_DBETAR(OHALF,unon)
         IF ( XPH(1).GE.xvqthr ) GOTO 50
         XPH(2) = ONE - XPH(1)
C x-fractions of Pomeron q-aq-pair
         xpolo = TINY2
         xpohi = ONE - TINY2
         XPPo(1) = DT_SAMPEX(xpolo,xpohi)
         XPPo(2) = ONE - XPPo(1)
C flavors of Pomeron q-aq-pair
         iflav = INT(ONE+DT_RNDM(unon)*(2.0D0+SEAsq))
         IFPpo(1) = iflav
         IFPpo(2) = -iflav
         IF ( DT_RNDM(unon).GT.OHALF ) THEN
            IFPpo(1) = -iflav
            IFPpo(2) = iflav
         END IF
      END IF
 
      IF ( Kt.EQ.2 ) THEN
C x-fractions of projectile target partons
 100     XTH(1) = DT_DBETAR(OHALF,unon)
         IF ( XTH(1).GE.xvqthr ) GOTO 100
         XTH(2) = ONE - XTH(1)
C x-fractions of Pomeron q-aq-pair
         xpolo = TINY2
         xpohi = ONE - TINY2
         XTPo(1) = DT_SAMPEX(xpolo,xpohi)
         XTPo(2) = ONE - XTPo(1)
C flavors of Pomeron q-aq-pair
         iflav = INT(ONE+DT_RNDM(xpolo)*(2.0D0+SEAsq))
         IFTpo(1) = iflav
         IFTpo(2) = -iflav
         IF ( DT_RNDM(xpolo).GT.OHALF ) THEN
            IFTpo(1) = -iflav
            IFTpo(2) = iflav
         END IF
      END IF
 
      END SUBROUTINE
