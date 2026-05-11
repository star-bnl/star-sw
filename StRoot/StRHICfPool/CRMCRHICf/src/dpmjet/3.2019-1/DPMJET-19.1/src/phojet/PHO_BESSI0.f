
      DOUBLE PRECISION FUNCTION PHO_BESSI0(X)
C**********************************************************************
C
C      Bessel Function I0
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ax , X , y
      SAVE 
 
      ax = ABS(X)
      IF ( ax.LT.3.75D0 ) THEN
         y = (X/3.75D0)**2
         PHO_BESSI0 = 1.0D0 + 
     &                y*(3.5156229D0+y*(3.0899424D0+y*(1.2067492D0+
     &                y*(0.2659732D0+y*(0.360768D-1+y*0.45813D-2)))))
      ELSE
         y = 3.75D0/ax
         PHO_BESSI0 = (EXP(ax)/SQRT(ax))
     &                *(0.39894228D0+y*(0.1328592D-1+y*
     &                (0.225319D-2+y*(-0.157565D-2+
     &                y*(0.916281D-2+y*(-0.2057706D-1+
     &                y*(0.2635537D-1+y*(-0.1647633D-1+y*0.392377D-2))))
     &                ))))
      END IF
 
      END FUNCTION
