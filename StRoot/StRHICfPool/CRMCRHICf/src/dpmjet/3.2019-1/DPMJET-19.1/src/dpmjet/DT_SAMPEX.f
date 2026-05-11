
      DOUBLE PRECISION FUNCTION DT_SAMPEX(X1,X2)
 
C***********************************************************************
C Sampling from f(x)=1./x between x1 and x2.                           *
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION al1 , al2 , DT_RNDM , ONE , r , X1 , X2
      SAVE 
      PARAMETER (ONE=1.0D0)
 
      r = DT_RNDM(X1)
      al1 = LOG(X1)
      al2 = LOG(X2)
      DT_SAMPEX = EXP((ONE-r)*al1+r*al2)
 
      END FUNCTION
