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
      integer nm,ne,ns, key/0/
      real eta,phi,nphi,r,z
      real offset(2) /1.3089969, 1.8325957/
      real sign(2)   /-1, 1/
** Must be getted from geometry of BEMC in GEANT => 20-feb-1999
      real smetawdh /0.9806/, seta1wdh /0.7277/, seta2wdh /0.9398/
     +   , set12wdh/0.04064/
      real rsmd(2)  /230.467,232.467/
      real shift_smde(2)

      if(key.eq.0) then
        key=1
        shift_smde(1) = 2.*smetawdh   + seta1wdh    ! The center of first eta strip
        shift_smde(2) = shift_smde(1) + (seta1wdh+set12wdh)*74*2  
     +  + (seta1wdh+set12wdh) + (seta2wdh+set12wdh)   ! The center of 76th eta strip
        write(6,*) ' <I> Shifts for SMDE ', shift_smde
     +           , ' R for SMD ', rsmd 
      endif
**
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
            z = shift_smde(1)+(seta1wdh+set12wdh)*2*(ebin-1) ! z-poz for center of ebin   
            r = rsmd(1)
         else
            z = shift_smde(2)+(seta2wdh+set12wdh)*2*(ebin-76)       ! z-poz for center of ebin  
            r = rsmd(2)  !Temporaly! Has to be check!!!
         endif
         eta = -1.*ALOG(TAN(ATAN2(r,z)/2.0))*rl
      elseif(det.eq.BSMDP) then                     ! Barrel SMD phi strip
         eta=rl*(ems_control(1).bemc_eta_bin(ebin*2-1)
     +          +ems_control(1).bemc_eta_bin(ebin*2+1))/2.0
      elseif(det.eq.ESMDE .or. det.eq.ESMDP) then   ! Endcap SMD phi strip
         eta= 0.0  ! we don't know
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

