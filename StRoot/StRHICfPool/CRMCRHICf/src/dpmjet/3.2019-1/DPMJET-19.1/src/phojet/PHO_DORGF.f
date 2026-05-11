
      DOUBLE PRECISION FUNCTION PHO_DORGF(X,S,Al,Be,Ak,Bk,Ag,Bg,C,D,E,
     &   Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , Be , Bg , Bk , C , D , E , Es , 
     &                 lx , S , sx , X
      SAVE 
 
      sx = SQRT(X)
      lx = LOG(1./X)
      PHO_DORGF = (X**Ak*(Ag+Bg*sx+C*X**Bk)
     &            +S**Al*EXP(-E+SQRT(Es*S**Be*lx)))*(1.-X)**D
 
      END FUNCTION
