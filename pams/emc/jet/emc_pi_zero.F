c:>------------------------------------------------------------------
C:FILE:         emc_pi_zero.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       K. Shestermanov, IHEP Protvino
C:BUGS:         None known.
C:HISTORY:      11/28/99  Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION emc_pi_zero(
     1           emc_gam_h,           emc_gam ,
     2       ems_control_h,       ems_control ,
     3   emc_part_id_par_h,   emc_part_id_par ,
     4        emc_pizero_h,        emc_pizero )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 emc_pi_zero
C:DESCRIPTION:  looks for pi_zero in barrel EMC
C:ARGUMENTS:    emc_gam,ems_control,
C:ARGUMENTS:    emc_cl_par,emc_cluster,emc_part_id_par,emc_pizero
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE

#include "emc_pi_zero.inc"
#include "emc_def.h"
#include "math_constants.inc"
#include "phys_constants.inc"
c
c %L
c
      integer ngam,i,j,pi
      real cx1(3),cx2(3),cxe1(3),cxe2(3),cpe(3)
      real eta1,s_eta1,phi1,s_phi1,eta2,s_eta2,phi2,s_phi2
      real egam1,s_egam1,egam2,s_egam2,cangle,vdot,sig_mass 
      real mass,s_mass,etap,phip,pt
c
      emc_pi_zero = STAFCV_OK
      emc_pizero_h.nok=0
      ngam=emc_gam_h.nok
      if(ngam.le.1) return
      do i=1,ngam
        eta1=emc_gam(i).eta
        phi1=emc_gam(i).phi
        s_eta1=emc_gam(i).sigma_eta
        s_phi1=emc_gam(i).sigma_phi
        call dirc(eta1,phi1,cx1)
        egam1=emc_gam(i).energy
        call vscale(cx1,egam1,cxe1,3)
        s_egam1=emc_gam(i).s_energy
        do j=i,ngam
          if(j.eq.i) go to 10
          emc_pizero_h.nok=emc_pizero_h.nok+1
          pi=emc_pizero_h.nok
          eta2=emc_gam(j).eta
          phi2=emc_gam(j).phi
          s_eta2=emc_gam(i).sigma_eta
          s_phi2=emc_gam(i).sigma_phi
          call dirc(eta2,phi2,cx2)
          egam2=emc_gam(j).energy
          call vscale(cx2,egam2,cxe2,3)
          s_egam2=emc_gam(j).s_energy
          call vadd(cxe1,cxe2,cpe,3)
          pt=sqrt(cpe(1)**2+cpe(2)**2)
          cangle=vdot(cx1,cx2,3)
          mass=sqrt(2.*egam1*egam2*(1.-cangle))
          s_mass=sig_mass(egam1,s_egam1,egam2,s_egam2,cangle)
          etap=-log(tan(atan2(pt,cpe(3))/2.))
          phip=atan2(cpe(2),cpe(1))
          do while(phip .ge. C_2PI)
            phip=phip-C_2PI
          enddo
          do while(phip .lt. 0.0)
            phip=phip+C_2PI
          enddo
c
c---fill emc_pizero table
c
          emc_pizero(pi).p_gamma1=i
          emc_pizero(pi).p_gamma2=j
          emc_pizero(pi).egam1=egam1
          emc_pizero(pi).egam2=egam2
          emc_pizero(pi).cosa=cangle
          emc_pizero(pi).pt=pt
          emc_pizero(pi).mass=mass
          emc_pizero(pi).sigma_mass=s_mass
          emc_pizero(pi).eta=etap
          emc_pizero(pi).phi=phip
c        
 10     enddo
      enddo
      emc_pi_zero=STAFCV_OK
999   RETURN
      END
c
      subroutine dirc(eta,phi,c)
c
      implicit none
c
      real c(3),eta,phi
c
      c(1)=cos(phi)/cosh(eta)
      c(2)=sin(phi)/cosh(eta)
      c(3)=tanh(eta)
c
      end
c
      real function sig_mass(e1,de1,e2,de2,c)
c
      real e1,de1,e2,de2,c
c
      sig_mass=sqrt(2.*(1.-c)*(e2/e1*de1**2+e1/e2*de2**2))
c
      end






















