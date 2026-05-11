
      DOUBLE PRECISION FUNCTION PHO_XLAM(X,Y,Z)
C**********************************************************************
C
C     auxiliary function for two/three particle decay mode
C     (standard LAMBDA**(1/2) function)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION X , xlam , Y , yz , Z
      SAVE 
C
      yz = Y - Z
      xlam = X*X - 2.D0*X*(Y+Z) + yz*yz
      IF ( xlam.LT.0.D0 ) xlam = -xlam
      PHO_XLAM = SQRT(xlam)
      END FUNCTION
