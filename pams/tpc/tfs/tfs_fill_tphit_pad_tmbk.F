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

      print *,"begin to run TFS_FILL_TPHITCLUS"
      nhits=tphit_h.nok

      do ihit=1,nhits

	if (tphit(ihit).flag.eq.0.or.tphit(ihit).flag.eq.1 )then 

c calculate the pads used by this hit
          xglobal(1)=tphit(ihit).x
          xglobal(2)=tphit(ihit).y
          xglobal(3)=tphit(ihit).z          
          isect=tphit(ihit).row/100
          longrow=tphit(ihit).row
          irow=mod(longrow,100)

          ret=tgc_global_to_local(jsect,xglobal,xlocal)

          if(jsect.ne.isect) 
     >   print *,'wrong sector number,isect=',isect,'jsect=',jsect
          row=irow
          ret=tgc_x_to_pad(row,xlocal(1),pad,pad_plane_h,pad_plane)

          ipad=pad

          sigma_x=tphit(ihit).prf
c        ret=tgc_x_to_pad(row,xlocal(1)-sigma_x*1.5,pad_min,pad_plane_h,pad_plane)
c        ret=tgc_x_to_pad(row,xlocal(1)+sigma_x*1.5,pad_max,pad_plane_h,pad_plane)
          ret=tgc_x_to_pad(row,xlocal(1)+sigma_x*1.5,pad_min,
     *        pad_plane_h,pad_plane)
          ret=tgc_x_to_pad(row,xlocal(1)-sigma_x*1.5,pad_max,
     *        pad_plane_h,pad_plane)

          min_pad=pad_min
          max_pad=pad_max

          npads=max_pad-min_pad+1

c          print *,"ihit=",ihit,"id=",tphit(ihit).id,
c     * "ihit=",ihit,
c     * "clus id=",tphit(ihit).id
c          print *,"sector=",isect,",irow=",irow
c          print *,"xlocal(1)=",xlocal(1),"ipad=",ipad
c          print *,"min_pad=",min_pad,"max_pad=",max_pad

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
       endif
      enddo

      tfs_fill_tphit_pad_tmbk=STAFCV_OK

      return 
      end
