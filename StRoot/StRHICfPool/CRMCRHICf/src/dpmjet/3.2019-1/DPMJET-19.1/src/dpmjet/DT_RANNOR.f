
      SUBROUTINE DT_RANNOR(X,Y)
 
C***********************************************************************
C Sampling from Gaussian distribution.                                 *
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a , cfe , DT_RNDM , sfe , TINY10 , v , X , Y
      SAVE 
      PARAMETER (TINY10=1.0D-10)
 
      CALL DT_DSFECF(sfe,cfe)
      v = MAX(TINY10,DT_RNDM(X))
      a = SQRT(-2.D0*LOG(v))
      X = a*sfe
      Y = a*cfe
 
      END SUBROUTINE
