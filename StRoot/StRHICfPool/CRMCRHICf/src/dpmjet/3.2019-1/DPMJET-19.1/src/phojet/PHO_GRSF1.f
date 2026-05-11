
      DOUBLE PRECISION FUNCTION PHO_GRSF1(X,S,Alp,Bet,A,B,Ga,Gb,Gc,Gd,
     &   Ge,Gep)
      IMPLICIT NONE
      DOUBLE PRECISION A , Alp , B , Bet , Ga , Gb , Gc , Gd , Ge , 
     &                 Gep , S , X
      SAVE 
 
      PHO_GRSF1 = (X**A*(Ga+Gb*SQRT(X)+Gc*X**B)
     &            +S**Alp*EXP(-Ge+SQRT(Gep*S**Bet*LOG(1.D0/X))))
     &            *(1.D0-X)**Gd
 
      END FUNCTION
