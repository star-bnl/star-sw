      subroutine volid_bemc(ivid,eta,phi,dep)
      implicit none
      integer emc_ivid(5)/10000000,100000,100,10,1/ ! matched with AGI and G2T
      integer ivid, eta, phi, dep, emc_chid(5), i, rl, sphi
      do i = 1,5
         emc_chid(i) = ivid/emc_ivid(i)
         ivid        = ivid - emc_chid(i)*emc_ivid(i)
      enddo
      if(ivid.ne.0)then
         type *,'VOLID_BEMC -- error decoding BEMC volume_id...',ivid
         STOP
      end if
      rl   = emc_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      eta  = emc_chid(2)        ! pseudorapidity bin number [1,20]
      phi  = emc_chid(3)        ! module phi [1,120]
      sphi = emc_chid(4)        ! d(eta)=0.1 tower number [1,10]
      dep  = emc_chid(5)        ! depth section [1,2]
      print *,'BEMC:rl,eta,phi,sphi,dep',rl,eta,phi,sphi,dep
      if(rl.eq.1) then
         phi=phi*2-2+sphi
      elseif(rl.eq.2) then
         eta=-eta
c         phi=(120-phi)*2+3-sphi
         phi=238-(120-phi)*2+sphi
      endif
      print *,'BEMC:eta,phi',eta,phi
      return
      end
