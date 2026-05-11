
      SUBROUTINE PHO_SWAPI(I1,I2)
C********************************************************************
C
C     exchange of argument values (integer)
C
C********************************************************************
      IMPLICIT NONE
      INTEGER I1 , I2 , k
      k = I1
      I1 = I2
      I2 = k
      END SUBROUTINE
