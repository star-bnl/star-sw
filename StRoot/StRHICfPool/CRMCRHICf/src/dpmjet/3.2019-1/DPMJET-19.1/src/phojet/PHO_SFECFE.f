
      SUBROUTINE PHO_SFECFE(Sfe,Cfe)
C**********************************************************************
C
C     fast random SIN(X) COS(X) selection
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Cfe , DT_RNDM , Sfe , x , xx , xy , y , yy
      SAVE 
C
 100  x = DT_RNDM(xx)
      y = DT_RNDM(yy)
      xx = x*x
      yy = y*y
      xy = xx + yy
      IF ( xy.GT.1.D0 ) GOTO 100
      Cfe = (xx-yy)/xy
      Sfe = 2.D0*x*y/xy
      IF ( DT_RNDM(xy).LT.0.5D0 ) Sfe = -Sfe
      END SUBROUTINE
