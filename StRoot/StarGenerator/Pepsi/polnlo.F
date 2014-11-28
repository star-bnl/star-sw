
C*********************************************************************
      
      SUBROUTINE POLNLO(IFLAG,X,Q2,
     &                  DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS)
      IMPLICIT NONE      
      DOUBLE PRECISION AUX(6),XD,Q2D
      DOUBLE PRECISION DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS      
      INTEGER IFLAG
      REAL X,Q2


c ---- NLO polarized parton distributions as described in
c ---- T. Gehrmann and W.J. Stirling:
c ---- Polarized Parton Distributions of the Nucleon
c ---- Durham preprint DTP/95/82
c ----
c ---- DXDUV = u-ubar
c ---- DXDDV = d-dbar 
c ---- DXDG  = gluon
c ---- DXDUBAR = 1/2 usea
c ---- DXDDBAR = 1/2 dsea
c ---- DXDS = sbar = 1/2 strsea

      XD=DBLE(X)
      Q2D=DBLE(Q2)

      call rdarrynlo(XD,Q2D,AUX,IFLAG)
      DXDUV=aux(1)*(XD**0.6d0*(1.d0-XD)**3)
      DXDDV=aux(2)*(XD**0.75d0*(1.d0-XD)**4)
      DXDG=aux(3)*(XD**0.5d0*(1.d0-XD)**5)
      DXDUBAR=aux(4)*(XD**0.5d0*(1.d0-XD)**6)
      DXDDBAR=aux(5)*(XD**0.5d0*(1.d0-XD)**6)
      DXDS=aux(6)*(XD**0.5d0*(1.d0-XD)**6)
      
      return
      end
