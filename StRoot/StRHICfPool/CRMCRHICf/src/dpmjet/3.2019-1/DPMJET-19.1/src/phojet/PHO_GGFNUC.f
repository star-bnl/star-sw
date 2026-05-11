
      DOUBLE PRECISION FUNCTION PHO_GGFNUC(W,Rho,Gamma)
C**********************************************************************
C
C      differential photonnumber for a nucleus (geometrical model)
C      (without form factor)
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Gamma , PHO_BESSK1 , PI , Rho , W , wgamma , 
     &                 wphib
      SAVE 
 
      PARAMETER (PI=3.141592653589793238462643383279D+00)
 
      wgamma = W/Gamma
      wphib = wgamma*PHO_BESSK1(wgamma*Rho)
 
      PHO_GGFNUC = 1.D0/PI**2*wphib**2
 
      END FUNCTION
