      subroutine find_bin_emc(ems_control,phi,eta,det,module,ebin,sub)
c*-
c*- Find ieta, iphi and detector for given eta, phi
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
      integer det,module,ebin,sub,iphi,nb,ne,p,s
      real eta, phi, etap, phip
      real offset(2) /1.3089969, 1.8325957/
      real sign(2)   /-1, 1/

      nb=ems_control(1).neta(BEMC)
      ne=ems_control(1).neta(EEMC)
      p=ems_control(1).nmodule(BEMC)*ems_control(1).nsub(BEMC)/2
      s=ems_control(1).nsub(BEMC)

      etap=abs(eta)
      if(eta.ge.0.0) then
         phip=sign(1)*(phi-offset(1))
      else
         phip=sign(2)*(phi-offset(2))
      endif
      do while(phip.ge.C_2PI)        
         phip=phip-C_2PI
      enddo
      do while(phip.lt.0.0)        
         phip=phip+C_2PI
      enddo

c*-Barrel
      if(etap.le.ems_control(1).bemc_eta_bin(nb+1)) then
         det=BEMC
         do ebin=1,nb
            if(etap.le.ems_control(1).bemc_eta_bin(ebin+1)) goto 10
         enddo
 10      continue
c*-Gap
      elseif(etap.lt.ems_control(1).eemc_eta_bin(1)) then
         det=0
         ebin=(-eta/etap+1)/2+1
c*-Endcap
      elseif(etap.le.ems_control(1).eemc_eta_bin(ne+1)) then
         det=EEMC
         do ebin=1,ne
            if(etap.le.ems_control(1).eemc_eta_bin(ebin+1)) goto 20
         enddo
 20      continue
         p=ems_control(1).nmodule(EEMC)*ems_control(1).nsub(EEMC)/2
         s=ems_control(1).nsub(EEMC)
c*-Outside EMC acceptance
      else                               
         det=-1
         ebin=(-eta/etap+1)/2+1
      endif

      iphi=int(phip/C_2PI*p)
      if(eta.lt.0.0) iphi=p+iphi
      module=iphi/s+1
      sub=mod(iphi,s)+1

      return
      end



