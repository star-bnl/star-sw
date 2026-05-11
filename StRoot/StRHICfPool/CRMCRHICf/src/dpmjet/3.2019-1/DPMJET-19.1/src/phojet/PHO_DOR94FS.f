
      DOUBLE PRECISION FUNCTION PHO_DOR94FS(X,S,Al,Be,Ak,Ag,B,D,E,Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , B , Be , D , dx , E , Es , lx , 
     &                 S , X
      SAVE 
 
      dx = SQRT(X)
      lx = LOG(1./X)
      PHO_DOR94FS = S**Al/lx**Ak*(1.+Ag*dx+B*X)*(1.-X)
     &              **D*EXP(-E+SQRT(Es*S**Be*lx))
 
      END FUNCTION
