      subroutine volid_esmd(ivid,module,eta,sub,type)
      implicit none
      INTEGER smd_ivid(4)/10000000,100000,100,1/   !!! check again -akio
      integer ivid,module,eta,sub,type,rl,smd_chid(4),i
      print *,'ESMD:ivid ',ivid
      do i = 1,4
         smd_chid(i) = ivid/smd_ivid(i)
         ivid        = ivid - smd_chid(i)*smd_ivid(i)
      enddo
      if(ivid.ne.0)then
         print *,'VOLID_ESMD -- error decoding ESMD volume_id...',ivid
         return
      end if
      rl     = smd_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      module = smd_chid(2)        ! pseudorapidity bin number [1,12]
      eta    = smd_chid(3)        !
      sub    = smd_chid(4)        !
      print *,'ESMD:rl,phi,sphi,seta',rl,module,eta,sub
      if(rl.eq.2)then
         module=25-module
      endif
      if(module.le.0) module=1
      if(eta.le.0)    eta=1
      if(sub.le.0)    sub=1
      if(type.le.0)   type=1
      return
      end
