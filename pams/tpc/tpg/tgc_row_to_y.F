* $Id: tgc_row_to_y.F,v 1.3 1998/09/11 16:15:48 sakrejda Exp $
* $Log: tgc_row_to_y.F,v $
* Revision 1.3  1998/09/11 16:15:48  sakrejda
* cleaned up so that linux compiler does not complain - no change in performance
*
* Revision 1.2  1998/01/27 01:56:43  fisyak
* Split sources
*
      integer function tgc_row_to_y(row,y,pad_plane_h,pad_plane)

      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)


      real row,y
C     Description: Converts row number to the y coordinate in the
C     local coordinate system
C     Input arguments:
C     row - row number
C     pad_plane - table with the pad plane parameters
C                 needed for initialisation
C     Returns:
C     y - y coordinate in the local coordinate system

      real row_cent(45)
      real row_wid_in, row_wid_out,wid,trunc_row,dif_row
      integer flag, i
      save
      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        row_wid_in=pad_plane(1).pad_len_in
        row_wid_out=pad_plane(1).pad_len_out
        flag=0
      endif
      trunc_row=int(row+0.5)

      if(trunc_row.lt.14.0) then
         wid=row_wid_in
      else
         wid=row_wid_out
      endif
      dif_row=row-trunc_row
      y=row_cent(int(trunc_row))+dif_row*wid
      tgc_row_to_y=STAFCV_OK
      end
