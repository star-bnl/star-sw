
      SUBROUTINE DT_DSFECF(Sfe,Cfe)
 
      IMPLICIT NONE
      DOUBLE PRECISION Cfe , DT_RNDM , OHALF , ONE , Sfe , TWO , x , 
     &                 xx , xy , y , yy , ZERO
      SAVE 
      PARAMETER (TWO=2.0D0,ONE=1.0D0,OHALF=0.5D0,ZERO=0.0D0)
 
 100  x = DT_RNDM(Sfe)
      y = DT_RNDM(x)
      xx = x*x
      yy = y*y
      xy = xx + yy
      IF ( xy.GT.ONE ) GOTO 100
      Cfe = (xx-yy)/xy
      Sfe = TWO*x*y/xy
      IF ( DT_RNDM(x).LT.OHALF ) Sfe = -Sfe
      END SUBROUTINE
