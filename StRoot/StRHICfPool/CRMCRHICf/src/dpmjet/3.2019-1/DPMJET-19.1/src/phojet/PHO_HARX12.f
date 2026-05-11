
      SUBROUTINE PHO_HARX12
C**********************************************************************
C
C     selection of x1 and x2 according to 1/x1*1/x2
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , TINY , TINY6 , z1 , z2
      SAVE 
 
      PARAMETER (TINY=1.D-30,TINY6=1.D-06)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
 
 100  z1 = Z1Max - DT_RNDM(X1)*Z1Dif
      z2 = Z2Max - DT_RNDM(X2)*Z2Dif
      IF ( (z1+z2).LT.ALNh ) GOTO 100
      X1 = EXP(z1)
      X2 = EXP(z2)
      AXX = AH/(X1*X2)
      W = SQRT(MAX(TINY,1.D0-AXX))
      W1 = AXX/(1.D0+W)
 
      END SUBROUTINE
