      subroutine find_pos_emc(ems_control,det,module,ebin,sub,phi,eta)
c*-
c*- Find eta, phi of the center of detectors
c*-
c*- det>0  for each detectors
c*-        see emc_def.h for module,ebin,sub definision
c*- det=0  for gap between barrel/endcap
c*- det=-1 for outside EMC
c*-        module, sub: same as BEMC    
c*-        ebin       : 1 (Z>0) or 2 (Z<0) 
c*-
      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
#include "emc_def.h"
#include "math_constants.inc"
      RECORD / ems_control_st / ems_control(*)
      integer det,module,ebin,sub,iphi,rl
      integer nm,ne,ns
      real eta,phi,nphi,r,z
      real offset(2) /1.3089969, 1.8325957/
      real sign(2)   /-1, 1/

      phi=0.0
      eta=0.0
      rl=1
      if(det.gt.0 .and. det.le.MAXDET) then
         nm=ems_control(1).nmodule(det)
         ne=ems_control(1).neta(det)
         ns=ems_control(1).nsub(det)
      else
         nm=ems_control(1).nmodule(BEMC)
         ne=2
         ns=ems_control(1).nsub(BEMC)         
      endif         
      if(module.le.0 .or. module.gt.nm) then
         write(*,*) '***find_pos_emc: module out of range: ',det,module
         return
      endif
      if(ebin.le.0 .or. ebin.gt.ne) then
         write(*,*) '***find_pos_emc: eta out of range: ',det,ebin
         return
      endif
      if(sub.le.0 .or. sub.gt.ns) then
         write(*,*) '***find_pos_emc: eta out of range: ',det,sub
         return
      endif      
      nphi=nm/2*ns
      if(module.le.nm/2) then
         iphi=(module-1)*ns+sub
         phi=(iphi-0.5)/nphi*C_2PI
         phi=sign(1)*phi+offset(1)
      else
         iphi=(module-nm/2-1)*ns+sub
         phi=(float(iphi)-0.5)/nphi*C_2PI
         phi=sign(2)*phi+offset(2)
         rl=-1
      endif
      do while(phi.ge.C_2PI)        
         phi=phi-C_2PI
      enddo
      do while(phi.lt.0.0)        
         phi=phi+C_2PI
      enddo

      if(det.eq.BEMC .or. det.eq.BPRS) then         ! Barrel tower/pre-shower
         eta=rl*(ems_control(1).bemc_eta_bin(ebin)
     +          +ems_control(1).bemc_eta_bin(ebin+1))/2.0
      elseif(det.eq.EEMC .or. det.eq.EPRS) then     ! Endcap tower/pre-shower
         eta=rl*(ems_control(1).eemc_eta_bin(ebin)
     +          +ems_control(1).eemc_eta_bin(ebin+1))/2.0
      elseif(det.eq.BSMDE) then                     ! Barrel SMD eta-strip
         if(ebin.le.75) then
            z=(ebin/75.0-0.5)*0.5
         else
            z=0.5 + ((ebin-75)/75.0-0.5)
     +       *ems_control(1).bemc_eta_bin(ebin+1)
         endif
         r=ems_control(1).bemc_inner_r              !Temporaly! Has to be check!!!
         z=z*ems_control(1).bemc_max_z              !Temporaly! Has to be check!!!
         eta=-1.0*ALOG(TAN(ATAN2(r,z)/2.0))
      elseif(det.eq.BSMDP) then                     ! Barrel SMD phi strip
         eta=rl*(ems_control(1).bemc_eta_bin(ebin*2-1)
     +          +ems_control(1).bemc_eta_bin(ebin*2+1))/2.0
      elseif(det.eq.0) then                         ! Gap
         eta=rl*(
     +        ems_control(1).bemc_eta_bin(ems_control(1).neta(BEMC)+1)
     +       +ems_control(1).eemc_eta_bin(1))/2.0
      elseif(det.le.0) then                         ! Outside
         eta=rl*10.0                                ! Put eta=+/-10
      else                                          ! Error
         write(*,*)'FIND_POS_EMC:Wrong detector:',det
         return
      endif
      
      return
      end

