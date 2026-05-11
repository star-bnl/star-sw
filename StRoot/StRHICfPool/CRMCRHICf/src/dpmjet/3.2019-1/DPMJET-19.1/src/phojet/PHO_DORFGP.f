
      DOUBLE PRECISION FUNCTION PHO_DORFGP(X,S,Al,Be,Ak,Bk,Ag,Bg,C,D,E,
     &   Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , Be , Bg , Bk , C , D , dx , E , 
     &                 Es , lx , S , X
      SAVE 
 
      dx = SQRT(X)
      lx = LOG(1./X)
      PHO_DORFGP = (X**Ak*(Ag+Bg*dx+C*X)
     &             *lx**Bk+S**Al*EXP(-E+SQRT(Es*S**Be*lx)))*(1.-X)**D
 
      END FUNCTION
