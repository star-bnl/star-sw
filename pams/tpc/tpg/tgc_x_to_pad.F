* $Id: tgc_x_to_pad.F,v 1.2 1998/01/27 01:56:44 fisyak Exp $
* $Log: tgc_x_to_pad.F,v $
* Revision 1.2  1998/01/27 01:56:44  fisyak
* Split sources
*
      integer function tgc_x_to_pad(row,x,pad,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real row, x, pad
C     Description: Converts x coordinate in the local coordinate system
C     to the pad number.
C     Input arguments:
C     row - row number
C     x - x coordinate in the local coordinate system
C     Returns:
C     pad - pad number 

      integer flag,i
      real pitch_in,pitch_out,pitch
      real pad_num(45)

      save

      data flag /1/
C
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
      pad = x/pitch+0.5+0.5*pad_num(int(row+0.5))
      tgc_x_to_pad=STAFCV_OK
      end
