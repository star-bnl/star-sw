      subroutine volid_bemc(ivid,module,eta,sub,dep,shift)
      implicit none
      integer emc_ivid(5)/10000000,100000,100,10,1/ ! matched with AGI and G2T
      integer ivid, module, eta, sub, dep, emc_chid(5), i, rl, phi
      real shift(2)

      do i = 1,5
         emc_chid(i) = ivid/emc_ivid(i)
         ivid        = ivid - emc_chid(i)*emc_ivid(i)
      enddo
      if(ivid.ne.0)then
         write(*,*) '***VOLID_BEMC -- error decoding BEMC volume_id...',ivid
         STOP
      end if
      rl     = emc_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      eta    = emc_chid(2)        ! pseudorapidity bin number [1,20]
      phi    = emc_chid(3)        ! module phi [1,120]
      sub    = emc_chid(4)        ! d(eta)=0.1 tower number [1,2]
      dep    = emc_chid(5)        ! depth section [1,2]
*      print *,'BEMC:rl,eta,phi,sub,dep',rl,eta,phi,sub,dep
*      print *,'shift  --->',shift
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
         sub=mod(sub+1,2)+1
      else
         write(*,*) '***VOLID_BSMD -- error decoding BSMD volume_id...',ivid
         STOP
      endif
*      print *,'BEMC:module,eta,sub,dep',module,eta,sub,dep
      return
      end
