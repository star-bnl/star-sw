      subroutine find_bin_emc(ems_control,rl,phi,eta,iphi,ieta)
c*-
c*- Find ieta, iphi and right/left bin for given eta, phi
c*- rl=3 should be given for barrel.
c*- rl=1 or 2 should be given for endcap, and return correct rl.
c*-
      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
      RECORD / ems_control_st / ems_control(*)
      COMMON/keep/PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad
      integer rl,ieta,iphi,n
      real eta, phi, etap, phip
      if(rl.eq.3) then
c*-Barrel
         n=ems_control(1).bemc_eta_nbin
         if(eta.le.ems_control(1).bemc_eta_bin(1)) then
            write(*,*) 'FIND_BIN_B: Eta out of range:', eta
            ieta=1
         elseif(eta.gt.ems_control(1).bemc_eta_bin(n+1)) then
            write(*,*) 'FIND_BIN_B: Eta out of range:', eta
            ieta=n
         else
            do ieta=1,n
               if(eta.le.ems_control(1).bemc_eta_bin(ieta+1))
     +            goto 10
            enddo
 10         continue
         endif         
         phip=(phi-ems_control(1).bemc_phi_offset)
     +       *ems_control(1).bemc_phi_sign
         do while(phip.ge.PI2)        
            phip=phip-PI2
         enddo
         do while(phip.lt.0.0)        
            phip=phip+PI2
         enddo
         iphi=int(phip/PI2*ems_control(1).bemc_phi_nbin)+1
      elseif(rl.eq.1 .or.rl.eq.2) then
c*-Endcap
         if(eta.gt.0.0)then 
            rl=1
         else
            rl=2
         endif
         n=ems_control(1).eemc_eta_nbin
         etap=abs(eta)
         if(etap.le.ems_control(1).eemc_eta_bin(1)) then
            write(*,*) 'FIND_BIN_E: Eta out of range:', eta
            ieta=1
         elseif(etap.gt.ems_control(1).eemc_eta_bin(n+1)) then
            write(*,*) 'FIND_BIN_E: Eta out of range:', eta
            ieta=n
         else
            do ieta=1,n
               if(etap.le.ems_control(1).eemc_eta_bin(ieta+1))
     +              goto 20
            enddo
 20         continue
         endif         
         phip=(phi-ems_control(1).eemc_phi_offset)
     +       *ems_control(1).eemc_phi_sign
         do while(phip.ge.PI2)        
            phip=phip-PI2
         enddo
         do while(phip.lt.0.0)        
            phip=phip+PI2
         enddo
         iphi=int(phip/PI2*ems_control(1).eemc_phi_nbin)+1
      else
         write(*,*)'FIND_BIN_EMC:Wrong detector:',rl
      endif
      return
      end
