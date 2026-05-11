
      DOUBLE PRECISION FUNCTION PHO_DORFQP(X,S,St,Al,Be,Ak,Ag,B,D,E,Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , B , Be , D , dx , E , Es , lx , 
     &                 S , St , X
      SAVE 
 
      dx = SQRT(X)
      lx = LOG(1./X)
      IF ( S.LE.St ) THEN
         PHO_DORFQP = 0.0
      ELSE
         PHO_DORFQP = (S-St)**Al/lx**Ak*(1.D0+Ag*dx+B*X)*(1.D0-X)
     &                **D*EXP(-E+SQRT(Es*S**Be*lx))
 
      END IF
      END FUNCTION
