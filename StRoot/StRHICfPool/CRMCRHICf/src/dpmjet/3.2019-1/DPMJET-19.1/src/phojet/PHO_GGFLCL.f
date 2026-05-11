
      DOUBLE PRECISION FUNCTION PHO_GGFLCL(Xi)
C*********************************************************************
C
C     semi-classical photon flux (geometrical model)
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION PHO_BESSK0 , PHO_BESSK1 , Xi
      SAVE 
 
      PHO_GGFLCL = 2.D0/3.141592653589793238462643383279D+00*
     &             (Xi*PHO_BESSK0(Xi)*PHO_BESSK1(Xi)
     &             -Xi**2/2.D0*(PHO_BESSK1(Xi)**2-PHO_BESSK0(Xi)**2))
 
      END FUNCTION
