      subroutine volid_bsmd(ivid,module,eta,sub,type,shift)
#include "emc_def.h"
      implicit none
      INTEGER smd_ivid(5)/100000000,1000000,1000,100,1/ ! matched with AGI&G2T
      integer ivid, module, eta, sub, type, smd_chid(5), i, rl, phi, t, strip
      real    shift(2)
      
      do i = 1,5
         smd_chid(i) = ivid/smd_ivid(i)
         ivid        = ivid - smd_chid(i)*smd_ivid(i)
      enddo
      if(ivid.ne.0)then
         write(*,*) '***VOLID_BSMD -- error decoding BSMD volume_id...',ivid
         return
      end if
      rl     = smd_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      eta    = smd_chid(2)        ! pseudorapidity bin number [-10,10]
      phi    = smd_chid(3)        ! module phi [1,60]
      t      = smd_chid(4)        ! SMD tipe 1->3
      strip  = smd_chid(5)        ! trip number 1-75(type 1,2) 1-15(type 3)
*      print *,'BSMD:rl,eta,phi,type,strip',rl,eta,phi,t,strip
      if(rl.eq.1) then
         phi=phi+(shift(1)-75)/6
         if(phi.le.0) then
            phi=phi+60
         elseif(phi.ge.61) then
            phi=phi-60
         endif
         module=phi
      elseif(rl.eq.2) then
         phi=phi+(shift(2)-105)/6
         if(phi.le.0) then
            phi=phi+60
         elseif(phi.ge.61) then
            phi=phi-60
         endif
         module=phi+60
      else
         write(*,*) '***VOLID_BSMD -- error decoding BSMD volume_id...',ivid
         return
      endif
      if(t.eq.1) then
         type=BSMDE
         eta=strip
         sub=1
      elseif(t.eq.2)then
         type=BSMDE
         eta=strip+75
         sub=1
      elseif(t.eq.3)then
         type=BSMDP
         eta=abs(eta)
         sub=strip
      else
         write(*,*) '***volid_bsmd.F: Type mismatch'
         return
      endif
*      print *,'BSMD:module,eta,type,strip',module,eta,phi,type,strip
      return
      end
