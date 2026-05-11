
      DOUBLE PRECISION FUNCTION PHO_BESSK0(X)
C**********************************************************************
C
C      Modified Bessel Function K0
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION PHO_BESSI0 , X , y
      SAVE 
 
      IF ( X.LT.2.D0 ) THEN
         y = X**2/4.D0
         PHO_BESSK0 = (-LOG(X/2.D0)*PHO_BESSI0(X))
     &                + (-.57721566D0+y*(0.42278420D0+
     &                y*(0.23069756D0+y*(0.3488590D-1+
     &                y*(0.262698D-2+y*(0.10750D-3+y*0.740D-5))))))
      ELSE
         y = 2.D0/X
         PHO_BESSK0 = (EXP(-X)/SQRT(X))
     &                *(1.25331414D0+y*(-0.7832358D-1+y*
     &                (0.2189568D-1+y*(-0.1062446D-1+
     &                y*(0.587872D-2+y*(-0.251540D-2+y*0.53208D-3))))))
      END IF
 
      END FUNCTION
