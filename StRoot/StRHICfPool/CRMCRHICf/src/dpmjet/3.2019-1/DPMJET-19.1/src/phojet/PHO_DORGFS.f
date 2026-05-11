
      DOUBLE PRECISION FUNCTION PHO_DORGFS(X,S,Sf,Al,Be,Ak,Bk,Ag,Bg,C,D,
     &   E,Es)
      IMPLICIT NONE
      DOUBLE PRECISION Ag , Ak , Al , Be , Bg , Bk , C , D , ds , E , 
     &                 Es , lx , S , Sf , sx , X
      SAVE 
 
      IF ( S.LE.Sf ) THEN
         PHO_DORGFS = 0.0
      ELSE
         sx = SQRT(X)
         lx = LOG(1./X)
         ds = S - Sf
         PHO_DORGFS = (ds*X**Ak*(Ag+Bg*sx+C*X**Bk)
     &                +ds**Al*EXP(-E+SQRT(Es*S**Be*lx)))*(1.-X)**D
 
      END IF
      END FUNCTION
