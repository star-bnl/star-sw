
      DOUBLE PRECISION FUNCTION PHO_BETAF(X1,X2,Bet)
C********************************************************************
C
C     weights of different quark flavours
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ax , ay , Bet , betx1 , X1 , X2
      SAVE 
 
      ax = 0.D0
      betx1 = Bet*X1
      IF ( betx1.LT.70.D0 ) ax = -1.D0/Bet**2*(betx1+1.D0)*EXP(-betx1)
      ay = 1.D0/Bet**2*(Bet*X2+1.D0)*EXP(-Bet*X2)
 
      PHO_BETAF = ax + ay
 
      END FUNCTION
