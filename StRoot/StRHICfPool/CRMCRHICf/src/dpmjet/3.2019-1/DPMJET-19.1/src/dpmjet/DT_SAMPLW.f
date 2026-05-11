
      DOUBLE PRECISION FUNCTION DT_SAMPLW(Xmin,Xmax,B)
 
C***********************************************************************
C Sampling from f(x)=1/x^b between x_min and x_max.                    *
C S. Roesler, 18.4.98                                                  *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION B , DT_RNDM , ONE , onemb , r , Xmax , Xmin
      SAVE 
      PARAMETER (ONE=1.0D0)
 
      r = DT_RNDM(B)
      IF ( B.EQ.ONE ) THEN
         DT_SAMPLW = EXP(r*LOG(Xmax)+(ONE-r)*LOG(Xmin))
      ELSE
         onemb = ONE - B
         DT_SAMPLW = (r*Xmax**onemb+(ONE-r)*Xmin**onemb)**(ONE/onemb)
      END IF
 
      END FUNCTION
