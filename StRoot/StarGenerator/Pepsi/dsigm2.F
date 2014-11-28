C **********************************************************************
      
      FUNCTION DSIGM2(X)
      
      IMPLICIT NONE
      
CAE...Modified version of DSIGMA with a factor 1/(1-XP) is removed
C...by a variable transformation. This makes the integration of the 
C...cross section faster.

      REAL X,XP,DSIGMA,DSIGM2
      
      XP=1.0-EXP(X)
      DSIGM2=(1.0-XP)*DSIGMA(XP)

      RETURN
      END
