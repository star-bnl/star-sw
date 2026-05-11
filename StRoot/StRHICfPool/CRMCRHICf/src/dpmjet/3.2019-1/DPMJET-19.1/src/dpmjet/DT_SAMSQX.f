
      DOUBLE PRECISION FUNCTION DT_SAMSQX(X1,X2)
 
C***********************************************************************
C Sampling from f(x)=1./x^0.5 between x1 and x2.                       *
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , ONE , r , X1 , X2
      SAVE 
      PARAMETER (ONE=1.0D0)
 
      r = DT_RNDM(X1)
      DT_SAMSQX = (r*SQRT(X2)+(ONE-r)*SQRT(X1))**2
 
      END FUNCTION
