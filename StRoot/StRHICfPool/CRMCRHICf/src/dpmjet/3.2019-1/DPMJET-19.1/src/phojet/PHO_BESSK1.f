
      DOUBLE PRECISION FUNCTION PHO_BESSK1(X)
C**********************************************************************
C
C      Modified Bessel Function K1
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION PHO_BESSI1 , X , y
      SAVE 
 
      IF ( X.LT.2.D0 ) THEN
         y = X**2/4.D0
         PHO_BESSK1 = (LOG(X/2.D0)*PHO_BESSI1(X)) + (1.D0/X)
     &                *(1.D0+y*(0.15443144D0+
     &                y*(-0.67278579D0+y*(-0.18156897D0+
     &                y*(-0.1919402D-1+y*(-0.110404D-2+y*(-0.4686D-4))))
     &                )))
      ELSE
         y = 2.D0/X
         PHO_BESSK1 = (EXP(-X)/SQRT(X))
     &                *(1.25331414D0+y*(0.23498619D0+y*(-0.3655620D-1+
     &                y*(0.1504268D-1+
     &                y*(-0.780353D-2+y*(0.325614D-2+y*(-0.68245D-3)))))
     &                ))
      END IF
 
      END FUNCTION
