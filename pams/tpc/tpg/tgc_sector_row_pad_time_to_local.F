* $Id: tgc_sector_row_pad_time_to_local.F,v 1.2 1998/01/27 01:56:43 fisyak Exp $
* $Log: tgc_sector_row_pad_time_to_local.F,v $
* Revision 1.2  1998/01/27 01:56:43  fisyak
* Split sources
*
      integer function tgc_sector_row_pad_time_to_local(isector,
     >pad,row,time,xlocal)
      implicit none
#include "PAM.inc"

      integer isector
      real    pad, row, time, xlocal(3)
C
C     Description: Conversion between the sector, row, pad, time to the local
C     coordinate system
C     Input arguments:
C     isector - sector number
C     pad     - pad number
C     row     - row number
C     time    - time bucket
C     Returns:
C     xlocal  - position of a pixel in local coordinate system
C 
      integer iret
      real    dummy1,dummy2
      integer tgc_pad_to_x
      integer tgc_row_to_y
      integer tgc_time_to_z
      iret=tgc_pad_to_x(pad,row,xlocal(1),dummy1,dummy2)
      iret=tgc_row_to_y(row,xlocal(2),dummy1,dummy2)
      iret=tgc_time_to_z(time,xlocal(3))
      tgc_sector_row_pad_time_to_local=STAFCV_OK
      end
