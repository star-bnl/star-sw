      subroutine find_bin_emc(ems_control,phi,eta,det,iphi,ieta)
c*-
c*- Find ieta, iphi and detector for given eta, phi
c*- det=1 for barrel
c*- det=2 for endcap
c*- det=3 gap between barrel and endcap
c*- det=4 out of acceptance (higer rapidity) 

      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
      RECORD / ems_control_st / ems_control(*)
      COMMON/keep/PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad
      integer det,ieta,iphi,nb,ne
      real eta, phi, etap, phip
      real offset,sign
      parameter (offset=1.3089969) !phi_bin=1(or 120) 
      parameter (sign=-1)           !anti-clockwise

      nb=ems_control(1).bemc_eta_nbin
      ne=ems_control(1).eemc_eta_nbin

      phip=sign*(phi-offset)
      do while(phip.ge.PI2)        
         phip=phip-PI2
      enddo
      do while(phip.lt.0.0)        
         phip=phip+PI2
      enddo

c*-Barrel
      if( eta.ge.ems_control(1).bemc_eta_bin(1) .and.
     +    eta.lt.ems_control(1).bemc_eta_bin(nb+1) ) then
         det=1
         do ieta=1,nb
            if(eta.lt.ems_control(1).bemc_eta_bin(ieta+1)) goto 10
         enddo
 10      continue
         if(eta.ge.0.0)then
            ieta=ieta-nb/2
         else
            ieta=-nb/2+ieta-1
         endif
         iphi=int(phip/PI2*ems_control(1).bemc_phi_nbin)+1
         if(eta.lt.0.0) iphi=2*ems_control(1).bemc_phi_nbin+1-iphi
c*-Gap
      elseif(abs(eta).lt.ems_control(1).eemc_eta_bin(1)) then
         det=3
         ieta=0
         iphi=int(phip/PI2*ems_control(1).bemc_phi_nbin)+1
         if(eta.lt.0.0) iphi=2*ems_control(1).bemc_phi_nbin+1-iphi

c*-Endcap
      elseif(abs(eta).lt.ems_control(1).eemc_eta_bin(ne+1)) then
         det=2
         etap=abs(eta)
         do ieta=1,ne
            if(etap.le.ems_control(1).eemc_eta_bin(ieta+1)) goto 20
         enddo
 20      continue
         if(eta.lt.0.0) ieta=-ieta
         phip=sign*(phi-offset)
         do while(phip.ge.PI2)        
            phip=phip-PI2
         enddo
         do while(phip.lt.0.0)        
            phip=phip+PI2
         enddo
         iphi=int(phip/PI2*ems_control(1).eemc_phi_nbin)+1
         if(eta.lt.0.0) iphi=2*ems_control(1).eemc_phi_nbin+1-iphi

c*-outside acceptance
      else
         det=4
         ieta=0
         iphi=int(phip/PI2*ems_control(1).eemc_phi_nbin)+1
         if(eta.lt.0.0) iphi=2*ems_control(1).eemc_phi_nbin+1-iphi
      endif
      return
      end
