      INTEGER FUNCTION TFS_FILL_TPHIT_PAD_TMBK( pad_plane_h,pad_plane,
     >                                          tphit_h,tphit)

      IMPLICIT NONE
#include "PAM.inc"
#include "tpg_pad_plane.inc"
#include "tcl_tphit.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)

      integer nhits, ihit
      integer ipad, npads,min_pad, max_pad
      integer itmbk,ntmbk,min_tmbk, max_tmbk
      integer isect, irow,jsect
      integer ret
      integer longrow

      integer tgc_global_to_local,tgc_x_to_pad,tgc_z_to_time
      real xglobal(3),xlocal(3)
      real sigma_x,sigma_z

      real row,pad,pad_min,pad_max

      print *,"begin to run TFS_FILL_TPHIT_PAD_TMBK"
      nhits=tphit_h.nok

      do ihit=1,nhits
c calculate the pads used by this hit
          xglobal(1)=tphit(ihit).x
          xglobal(2)=tphit(ihit).y
          xglobal(3)=tphit(ihit).z          
          isect=tphit(ihit).row/100
          longrow=tphit(ihit).row
          irow=mod(longrow,100)

          ret=tgc_global_to_local(jsect,xglobal,xlocal)

c          if(jsect.ne.isect) 
c     >   print *,'wrong sector number,isect=',isect,'jsect=',jsect
          row=irow
          ret=tgc_x_to_pad(row,xlocal(1),pad,pad_plane_h,pad_plane)
          
c the following conventions:
c http://www.star.bnl.gov/STAR/html/starsas_mail_l/0010.html
c for pad #1   x_to_pad   would return   numbers between 0.5 and 1.5
c (this actually came from na49)
          ipad=int(pad+0.5)

          if(ipad.le.0.or.ipad.gt.pad_plane(1).npads(irow))then
c             print *,'wrong hit location,mark it flag=14,ihit=',ihit,
c     >  ',old flag=',tphit(ihit).flag
            tphit(ihit).flag=14
          endif
 
          sigma_x=tphit(ihit).prf

          ret=tgc_x_to_pad(row,xlocal(1)+sigma_x*1.5,pad_min,
     *        pad_plane_h,pad_plane)
          ret=tgc_x_to_pad(row,xlocal(1)-sigma_x*1.5,pad_max,
     *        pad_plane_h,pad_plane)

          min_pad=int(pad_min+0.5)
          max_pad=int(pad_max+0.5)

          if(min_pad.le.0.or.
     > min_pad.gt.pad_plane(1).npads(irow))then
             tphit(ihit).flag=-14
             min_pad=1
          endif

          if(max_pad.le.0.or.
     >  max_pad.gt.pad_plane(1).npads(irow)) then
             tphit(ihit).flag=-14
             max_pad=pad_plane(1).npads(irow)
          endif
          
          npads=max_pad-min_pad+1
          if(npads.le.0.or.npads.gt.pad_plane(1).npads(irow))then
c              print *,'wrong npads, mark it to flag=14, npad=',npads,
c     >   ',ihit=',ihit,',old flag=',tphit(ihit).flag
             tphit(ihit).flag=14
          endif              

          tphit(ihit).npads=npads
          tphit(ihit).minpad=min_pad
          tphit(ihit).maxpad=max_pad

c calculate the tmbks used by this hit

          ret=tgc_z_to_time(xlocal(3),ipad,irow,isect,itmbk)

          sigma_z=tphit(ihit).zrf
          ret=tgc_z_to_time(xlocal(3)-1.5*sigma_z,ipad,
     *        irow,isect,min_tmbk)
          ret=tgc_z_to_time(xlocal(3)+1.5*sigma_z,ipad,
     *        irow,isect,max_tmbk)

          ntmbk=max_tmbk-min_tmbk+1


          tphit(ihit).ntmbk=ntmbk
          tphit(ihit).mintmbk=min_tmbk
          tphit(ihit).maxtmbk=max_tmbk
c       endif
      enddo

      tfs_fill_tphit_pad_tmbk=STAFCV_OK

      return 
      end
