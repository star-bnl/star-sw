* $Id: tgc_local_to_row_pad_time.F,v 1.2 1998/01/27 01:56:42 fisyak Exp $
* $Log: tgc_local_to_row_pad_time.F,v $
* Revision 1.2  1998/01/27 01:56:42  fisyak
* Split sources
*
      integer function tgc_local_to_row_pad_time(xlocal,
     >pad,row,time)
      implicit none
#include "PAM.inc"

      real xlocal(3)
      real pad, row, time

C     Description: Converts local coordinates to pad number
C     row number and time bucket
C     Input arguments:
C     xlocal - local coordinates
C     Returns:
C     pad - pad number
C     row - row number
C     time - time bucket number
C
      real dummy1, dummy2
      integer iret
      integer tgc_x_to_pad, tgc_y_to_row 
      
      time=0
      iret=tgc_x_to_pad(row,xlocal(1),pad,dummy1,dummy2)
      iret=tgc_y_to_row(xlocal(2),row,dummy1,dummy2)
      tgc_local_to_row_pad_time=STAFCV_OK
      end
