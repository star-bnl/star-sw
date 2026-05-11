
      DOUBLE PRECISION FUNCTION PHO_DOR92FW(X,S,Al,Be,Ak,Bk,Ag,Bg,C,D,E,
     &   Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , Be , Bg , Bk , C , D , E , Es , 
     &                 lx , S , X
      SAVE 
      lx = LOG(1./X)
      PHO_DOR92FW = (X**Ak*(Ag+X*(Bg+X*C))
     &              *lx**Bk+S**Al*EXP(-E+SQRT(Es*S**Be*lx)))*(1.-X)**D
 
      END FUNCTION
