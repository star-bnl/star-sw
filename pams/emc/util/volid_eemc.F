      subroutine volid_eemc(ivid,module,eta,sub,dep)
      implicit none
      integer eemc_ivid(4)/100000,1000,10,1/  !!! check again-akio
      integer ivid, module, eta, sub, dep, emc_chid(4), i, rl, phi
      do i = 1,4
         emc_chid(i) = ivid/eemc_ivid(i)
         ivid        = ivid - emc_chid(i)*eemc_ivid(i)
      enddo
      if(ivid.ne.0)then
         type *,'VOLID_EEMC -- error decoding EEMC volume_id...',ivid
         return
      endif
      rl  = emc_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      phi = emc_chid(2)        ! module phi [1,60]
      eta = emc_chid(3)        ! pseudorapidity bin number [1,12]
      dep = emc_chid(4)        ! depth section [1,4]
      eta=13-eta
      module=(phi-1)/5+1
      sub=mod(phi-1,5)+1
      if(rl.eq.2) then
         module=25-module
         sub=6-sub
      endif
      if(dep.gt.2) then
         dep=2
      endif
c      print *,'EEMC:rl,module,eta,sub,dep',rl,module,eta,dep
      return
      end
