      subroutine find_pos_emc(ems_control,det,iphi,ieta,phi,eta)
c*-
c*- Find eta, phi of the center of tower det, ieta, iphi
c*- det=3 for barrel and det=2 for endcap
c*-
      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
      RECORD / ems_control_st / ems_control(*)
      COMMON/keep/PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad
      integer det,ieta,iphi,aeta,aphi,rl,ne,np
      real eta,phi,offset,sign
      parameter (offset=1.3089969) !phi_bin=1(or 120) 
      parameter (sign=-1)          !anti-clockwise

      if(ieta.eq.0) then
         write(*,*) '***find_pos_emc: eta out of range: ',det,ieta
         phi=0.0
         eta=0.0
         return
      endif      
      aeta=abs(ieta)
      rl=ieta/aeta

c*- Barrel
      if(det.eq.1)then
         ne=ems_control(1).bemc_eta_nbin
         np=ems_control(1).bemc_phi_nbin
         if(rl.eq.1) then
            aeta=ne/2+ieta
            aphi=iphi
         else
            aeta=ne/2+ieta+1
            aphi=np*2+1-iphi
         endif
         if(aeta.lt.1 .or. aeta.gt.ne) then
            write(*,*) '***find_pos_emc: eta out of range: ',det,ieta,aeta
            return
         endif
         if(aphi.lt.1 .or. aphi.gt.np) then
            write(*,*) '***find_pos_emc: phi out of range: ',det,ieta,iphi,aeta,aphi
            return
         endif
         eta=rl*(ems_control(1).bemc_eta_bin(aeta)
     +          +ems_control(1).bemc_eta_bin(aeta+1))/2.0
         phi=sign*(float(aphi)-0.5)*PI2/np+offset

c*- Endcap
      elseif(det.eq.2) then     !Endcap
         ne=ems_control(1).eemc_eta_nbin
         np=ems_control(1).eemc_phi_nbin
         if(aeta.lt.1 .or. aeta.gt.ne) then
            write(*,*) '***find_pos_emc: eta out of range: ',det,ieta
            return
         endif
         if(rl.eq.1) then
            aphi=iphi
         else
            aphi=np*2+1-iphi
         endif
         if(aphi.le.0 .or. aphi.gt.np) then
            write(*,*) '***find_pos_emc: phi out of range: ',det,ieta,iphi
            return
         endif
         eta=rl*(ems_control(1).eemc_eta_bin(aeta)
     +          +ems_control(1).eemc_eta_bin(aeta+1))/2.0
         phi=sign*(float(aphi)-0.5)*PI2/np+offset

c-* error
      else
         write(*,*)'FIND_POS_EMC:Wrong detector:',det
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

