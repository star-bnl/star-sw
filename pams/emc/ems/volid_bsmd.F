      subroutine volid_bsmd(ivid,eta,phi,type,strip,shift)
      implicit none
      INTEGER smd_ivid(5)/100000000,1000000,1000,100,1/ ! matched with AGI&G2T
      integer ivid, eta, phi, type, strip, smd_chid(5), i, rl, sphi
      real    shift

      do i = 1,5
         smd_chid(i) = ivid/smd_ivid(i)
         ivid        = ivid - smd_chid(i)*smd_ivid(i)
      enddo
      if(ivid.ne.0)then
         type *,'VOLID_BSMD -- error decoding BSMD volume_id...',ivid
         STOP
      end if
      rl   = smd_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      eta  = smd_chid(2)        ! pseudorapidity bin number [-10,10]
      phi  = smd_chid(3)        ! module phi [1,60]
      type = smd_chid(4)
      strip= smd_chid(5)
c      print *,'BSMD:rl,eta,phi,type,strip',rl,eta,phi,type,strip
      if(rl.eq.0) then
         rl=2
         phi=phi+(shift-75)/6
         if(phi.gt.120) then
            phi=phi-60
         elseif(phi.le.60) then
            phi=phi+60
         endif
      endif
      if(rl.eq.2) then
         eta=-eta
      endif
c      print *,'BSMD:eta,phi',eta,phi
      return
      end
