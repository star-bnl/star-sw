
      DOUBLE PRECISION FUNCTION PHO_RNDBET(Gam,Eta)
C********************************************************************
C
C     RANDOM NUMBER GENERATION FROM BETA
C     DISTRIBUTION IN REGION  0 < X < 1.
C     F(X) = X**(GAM-1.)*(1.-X)**(ETA-1)*GAMM(ETA+GAM) / (GAMM(GAM
C                                                        *GAMM(ETA))
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Eta , Gam , PHO_RNDGAM , y , z
      SAVE 
 
      y = PHO_RNDGAM(1.D0,Gam)
      z = PHO_RNDGAM(1.D0,Eta)
 
      PHO_RNDBET = y/(y+z)
 
      END FUNCTION
