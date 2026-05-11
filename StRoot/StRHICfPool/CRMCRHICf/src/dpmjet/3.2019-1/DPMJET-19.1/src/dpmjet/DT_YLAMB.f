
      DOUBLE PRECISION FUNCTION DT_YLAMB(X,Y,Z)
 
C***********************************************************************
C                                                                      *
C     auxiliary function for three particle decay mode                 *
C     (standard LAMBDA**(1/2) function)                                *
C                                                                      *
C Adopted from an original version written by R. Engel.                *
C This version dated 12.12.94 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION X , xlam , Y , yz , Z
      SAVE 
 
      yz = Y - Z
      xlam = X*X - 2.D0*X*(Y+Z) + yz*yz
      IF ( xlam.LE.0.D0 ) xlam = ABS(xlam)
      DT_YLAMB = SQRT(xlam)
 
      END FUNCTION
