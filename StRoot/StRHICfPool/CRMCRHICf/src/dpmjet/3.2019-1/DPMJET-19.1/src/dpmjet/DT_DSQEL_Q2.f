
      DOUBLE PRECISION FUNCTION DT_DSQEL_Q2(Jtyp,Enu,Q2)
 
C...differential cross section for  Quasi-Elastic scattering
C.       nu + N -> l + N'
C.  From Llewellin Smith  Phys.Rep.  3C, 261, (1971).
C.
C.  INPUT :  JTYP = 1,...,6    nu_e, ...., nubar_tau
C.           ENU (GeV) =  Neutrino energy
C.           Q2  (GeV**2) =  (Transfer momentum)**2
C.
C.  OUTPUT : DSQEL_Q2  = differential  cross section :
C.                       dsigma/dq**2  (10**-38 cm+2/GeV**2)
C------------------------------------------------------------------
 
      IMPLICIT NONE
      DOUBLE PRECISION a1 , a2 , aa , axial2 , bb , c0 , cc , csi , 
     &                 Enu , fa , fa0 , ffa , ffv1 , ffv2 , fv1 , fv2 , 
     &                 gve , gvm , Q2 , rm
      DOUBLE PRECISION ss , su , x , xa
      INTEGER Jtyp
      SAVE 
 
C particle masses used in qel neutrino scattering modules
      INCLUDE 'inc/qnmass'
C*sr - removed (not needed)
C     COMMON /CAXIAL/ FA0, AXIAL2
C*
 
      DIMENSION ss(6)
      DATA c0/0.17590D0/    ! G_F**2 cos(theta_c)**2 M**2 /(8 pi) 10**-38 cm+2
      DATA ss/1.D0 , -1.D0 , 1.D0 , -1.D0 , 1.D0 , -1.D0/
      DATA axial2/1.03D0/   ! to be checked
 
      fa0 = -1.253D0
      csi = 3.71D0                   !  ???
      gve = 1.D0/(1.D0+Q2/0.84D0**2)**2      ! G_e(q**2)
      gvm = (1.D0+csi)*gve           ! G_m (q**2)
      x = Q2/(EMN*EMN)     ! emn=massa barione
      xa = x/4.D0
      fv1 = 1.D0/(1.D0+xa)*(gve+xa*gvm)
      fv2 = 1.D0/(1.D0+xa)*(gvm-gve)
      fa = fa0/(1.D0+Q2/axial2)**2
      ffa = fa*fa
      ffv1 = fv1*fv1
      ffv2 = fv2*fv2
      rm = EMLsq(Jtyp)/(EMN*EMN)            ! emlsq(jtyp)
      a1 = (4.D0+x)*ffa - (4.D0-x)*ffv1 + x*ffv2*(1.D0-xa) + 4*x*fv1*fv2
      a2 = -rm*((fv1+fv2)**2+ffa)
      aa = (xa+0.25D0*rm)*(a1+a2)
      bb = -x*fa*(fv1+fv2)
      cc = 0.25D0*(ffa+ffv1+xa*ffv2)
      su = (4.D0*Enu*EMN-Q2-EMLsq(Jtyp))/(EMN*EMN)
      DT_DSQEL_Q2 = c0*(aa+ss(Jtyp)*bb*su+cc*su*su)/(Enu*Enu)        !
      IF ( DT_DSQEL_Q2.LT.0.D0 ) DT_DSQEL_Q2 = 0.D0
 
      END FUNCTION
