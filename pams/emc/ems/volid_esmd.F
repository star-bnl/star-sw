      subroutine volid_esmd(ivid,rl,phi,sphi,seta)
      implicit none
      INTEGER smd_ivid(4)/10000000,100000,100,1/   !!! check again -akio
      integer ivid, rl, phi, sphi, seta, smd_chid(4), i
      do i = 1,4
         smd_chid(i) = ivid/smd_ivid(i)
         ivid        = ivid - smd_chid(i)*smd_ivid(i)
      enddo
      if(ivid.ne.0)then
         type *,'VOLID_BSMD -- error decoding BSMD volume_id...',ivid
         STOP
      end if
      rl   = smd_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
      phi  = smd_chid(2)        ! pseudorapidity bin number [1,12]
      sphi = smd_chid(3)        !
      seta = smd_chid(4)        !
c      print *,'ESMD:rl,phi,sphi,seta',rl,phi,sphi,seta
      return
      end
