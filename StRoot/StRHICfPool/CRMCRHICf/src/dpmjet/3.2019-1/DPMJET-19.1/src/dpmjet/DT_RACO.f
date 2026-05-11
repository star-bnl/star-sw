
      SUBROUTINE DT_RACO(Wx,Wy,Wz)
 
C***********************************************************************
C Direction cosines of random uniform (isotropic) direction in three   *
C dimensional space                                                    *
C Processed by S. Roesler, 20.11.95                                    *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION cfe , DT_RNDM , OHALF , ONE , sfe , TWO , Wx , 
     &                 Wy , Wz , x , x2 , y , y2 , z , ZERO
      SAVE 
      PARAMETER (TWO=2.0D0,ONE=1.0D0,OHALF=0.5D0,ZERO=0.0D0)
 
 100  x = TWO*DT_RNDM(Wx) - ONE
      y = DT_RNDM(x)
      x2 = x*x
      y2 = y*y
      IF ( x2+y2.GT.ONE ) GOTO 100
 
      cfe = (x2-y2)/(x2+y2)
      sfe = TWO*x*y/(x2+y2)
C z = 1/2 [ 1 + cos (theta) ]
      z = DT_RNDM(x)
C 1/2 sin (theta)
      Wz = SQRT(z*(ONE-z))
      Wx = TWO*Wz*cfe
      Wy = TWO*Wz*sfe
      Wz = TWO*z - ONE
 
      END SUBROUTINE
