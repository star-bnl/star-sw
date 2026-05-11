
      DOUBLE PRECISION FUNCTION PHO_DORFVP(X,N,Ak,Ag,D)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , D , dx , N , X
      SAVE 
 
      dx = SQRT(X)
      PHO_DORFVP = N*X**Ak*(1.+Ag*dx)*(1.-X)**D
 
      END FUNCTION
