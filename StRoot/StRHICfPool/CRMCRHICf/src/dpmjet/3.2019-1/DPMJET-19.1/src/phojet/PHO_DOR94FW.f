
      DOUBLE PRECISION FUNCTION PHO_DOR94FW(X,S,Al,Be,Ak,Bk,A,B,C,D,E,
     &   Es)
      IMPLICIT NONE
      DOUBLE PRECISION A , Ak , Al , B , Be , Bk , C , D , E , Es , lx , 
     &                 S , X
      SAVE 
 
      lx = LOG(1./X)
      PHO_DOR94FW = (X**Ak*(A+X*(B+X*C))
     &              *lx**Bk+S**Al*EXP(-E+SQRT(Es*S**Be*lx)))*(1.-X)**D
 
      END FUNCTION
