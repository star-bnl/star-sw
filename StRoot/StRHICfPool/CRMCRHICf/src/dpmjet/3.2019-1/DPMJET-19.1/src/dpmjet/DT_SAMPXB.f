
      DOUBLE PRECISION FUNCTION DT_SAMPXB(X1,X2,B)
 
C***********************************************************************
C Sampling from f(x)=1./SQRT(X**2+B**2) between x1 and x2.             *
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a , a1 , a2 , an , B , bb , DT_RNDM , TWO , X1 , 
     &                 X2
      SAVE 
      PARAMETER (TWO=2.0D0)
 
      a1 = LOG(X1+SQRT(X1**2+B**2))
      a2 = LOG(X2+SQRT(X2**2+B**2))
      an = a2 - a1
      a = an*DT_RNDM(a1) + a1
      bb = EXP(a)
      DT_SAMPXB = (bb**2-B**2)/(TWO*bb)
 
      END FUNCTION
