
      DOUBLE PRECISION FUNCTION PHO_DOR92FV(X,N,Ak,Ag,B,D)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , B , D , dx , N , X
      SAVE 
      dx = SQRT(X)
      PHO_DOR92FV = N*X**Ak*(1.+Ag*dx+B*X)*(1.-X)**D
 
      END FUNCTION
