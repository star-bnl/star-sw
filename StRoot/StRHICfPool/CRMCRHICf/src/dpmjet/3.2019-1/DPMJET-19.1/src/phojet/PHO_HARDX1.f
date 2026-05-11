
      SUBROUTINE PHO_HARDX1
C**********************************************************************
C
C     selection of x1 according to 1/x1
C     ( x2 = 1 )
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , TINY , TINY6 , z1
      SAVE 
 
      PARAMETER (TINY=1.D-30,TINY6=1.D-06)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
 
      z1 = Z1Max - DT_RNDM(X1)*Z1Dif
      X2 = 1.D0
      X1 = EXP(z1)
      AXX = AH/X1
      W = SQRT(MAX(TINY,1.D0-AXX))
      W1 = AXX/(1.D0+W)
 
      END SUBROUTINE
