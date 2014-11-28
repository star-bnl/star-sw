
*72*********************************************************************

      SUBROUTINE POLPAR(IFLAG,X,Q2,UVAL,DVAL,GLUE,QBAR,STR)

      IMPLICIT NONE

      INTEGER IFLAG
      DOUBLE PRECISION X,Q2,UVAL,DVAL,GLUE,QBAR,STR,AUX(5)

c ---- LO polarized parton distributions as described in
c ----    T. Gehrmann and W.J. Stirling:
c ----    "Polarized Parton Distributions of the Nucleon"
c ----    Durham preprint DTP/95/82
c ---- uval = u-ubar
c ---- dval = d-dbar 
c ---- glue 
c ---- qbar = ubar = dbar = 1/2 usea = 1/2 dsea
c ---- str = sbar = 1/2 strsea
 
      call rdarry(x,q2,aux,iflag)
      uval=aux(1)*(x**0.6d0*(1.d0-x)**3)
      dval=aux(2)*(x**0.75d0*(1.d0-x)**4)
      glue=aux(3)*(x**0.5d0*(1.d0-x)**5)
      qbar=aux(4)*(x**0.5d0*(1.d0-x)**6)
      str=aux(5)*(x**0.5d0*(1.d0-x)**6)
 
      return
      end
