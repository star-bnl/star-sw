
      SUBROUTINE PHO_SWAPD(D1,D2)
C********************************************************************
C
C     exchange of argument values (double precision)
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION d , D1 , D2
      d = D1
      D1 = D2
      D2 = d
      END SUBROUTINE
