* $Id: tgc_pad_to_x.F,v 1.2 1998/01/27 01:56:42 fisyak Exp $
* $Log: tgc_pad_to_x.F,v $
* Revision 1.2  1998/01/27 01:56:42  fisyak
* Split sources
*
      integer function tgc_pad_to_x(pad,row,x,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*) 

      real pad, row, x

C     Description: Converts pad and row number to the x
C     coordinate in the local coordinate system
C     Returns:
C     x - x coordinate in the local coordinate system

      integer flag,i
      real pitch_in,pitch_out,pitch
      real pad_num(45)
      real pad_int, pad_half, pad_dif
      save

      data flag /1/
      if(flag.eq.1) then
         pitch_in=pad_plane(1).pad_sep_in
         pitch_out=pad_plane(1).pad_sep_out
         do i=1,45
           pad_num(i)=pad_plane(1).npads(i)
         enddo
         flag=0
      endif
      if(row.lt.13.5) then
          pitch=pitch_in
      else
          pitch=pitch_out
      endif
      pad_int=int(pad+0.5)
      pad_half=pad_num(int(row+0.5))/2.0
      pad_dif=pad-pad_int
      x=(pad_int-pad_half-0.5+pad_dif)*pitch
      tgc_pad_to_x=STAFCV_OK
      end
