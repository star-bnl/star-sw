      subroutine find_pos_emc(ems_control,rl,iphi,ieta,phi,eta)
c*-
c*- Find eta, phi of the center of tower rl, ieta, iphi
c*- rl=3 for barrel and rl=1and2 for endcap
c*-
      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
      RECORD / ems_control_st / ems_control(*)
      COMMON/keep/PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad
      integer rl,ieta,iphi
      real eta,phi
      if(rl.eq.3)then
c*-Barrel
         eta=(ems_control(1).bemc_eta_bin(ieta)
     +       +ems_control(1).bemc_eta_bin(ieta+1))/2.0
         phi=(float(iphi)-0.5)*PI2
     +      /ems_control(1).bemc_phi_nbin
     +      *ems_control(1).bemc_phi_sign
     +      +ems_control(1).bemc_phi_offset
      elseif(rl.eq.1 .or. rl.eq.2) then
c*-Endcap         
         eta=(ems_control(1).eemc_eta_bin(ieta)
     +      +ems_control(1).eemc_eta_bin(ieta+1))/2.0
     +      *(3.0-2.0*float(rl))
         phi=(float(iphi)-0.5)*PI2
     +      /ems_control(1).eemc_phi_nbin
     +      *ems_control(1).eemc_phi_sign
     +      +ems_control(1).eemc_phi_offset
      else
         write(*,*)'FIND_POS_EMC:Wrong detector:',rl
         return
      endif
      do while(phi.ge.PI2)
         phi=phi-PI2
      enddo
      do while(phi.lt.0.0)
         phi=phi+PI2
      enddo
      return
      end
