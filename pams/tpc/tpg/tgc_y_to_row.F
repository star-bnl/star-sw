* $Id: tgc_y_to_row.F,v 1.2 1998/01/27 01:56:44 fisyak Exp $
* $Log: tgc_y_to_row.F,v $
* Revision 1.2  1998/01/27 01:56:44  fisyak
* Split sources
*
      integer function tgc_y_to_row(y,row,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real y, row
C     Description: Converts y to row number
C     Input arguments:
C     y - y in local coordinate system
C     Returns:
C     row - row number

      integer flag, i
      real row_cent(45)
      real row_wid_in, row_wid_out,dif_row
      real row_wid(45)
      save

      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        row_wid_in=pad_plane(1).pad_len_in
        row_wid_out=pad_plane(1).pad_len_out
        do i=1,13
           row_wid(i)=row_wid_in*0.5
        end do
        do i=14,45
           row_wid(i)=row_wid_out*0.5
        end do
        flag=0
      endif
      do i=1,45
         dif_row=y-row_cent(i)
         if(abs(dif_row).le.row_wid(i))then
           row=real(i)+0.5*dif_row/row_wid(i)
           go to 50
         endif
      end do
50    continue
      tgc_y_to_row=STAFCV_OK
      end
