      subroutine volid_eemc(ivid,eta,phi,dep)
      implicit none
      integer eemc_ivid(4)/100000,1000,10,1/  !!! check again-akio
      integer ivid, eta, phi, dep, emc_chid(4), i, rl
      do i = 1,4
         emc_chid(i) = ivid/eemc_ivid(i)
         ivid        = ivid - emc_chid(i)*eemc_ivid(i)
      enddo
      if(ivid.ne.0)then
         type *,'VOLID_BEMC -- error decoding EEMC volume_id...',ivid
         STOP
      end if
      rl   = emc_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      phi  = emc_chid(2)        ! module phi [1,60]
      eta  = emc_chid(3)        ! pseudorapidity bin number [1,12]
      dep  = emc_chid(4)        ! depth section [1,4]
c      print *,'EEMC:rl,eta,phi,dep',rl,eta,phi,dep
      if(rl.eq.1) then
         eta=13-eta
      else
         eta=eta-13
         phi=121-phi
      endif
      return
      end
