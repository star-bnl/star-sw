* $Id: tgc_row_par.F,v 1.2 1998/01/27 01:56:42 fisyak Exp $
* $Log: tgc_row_par.F,v $
* Revision 1.2  1998/01/27 01:56:42  fisyak
* Split sources
*
      integer function tgc_row_par(isect,row,a,b,pad_plane_h,pad_plane)
      implicit none

#include "PAM.inc"
#include "tpg_pad_plane.inc"

      RECORD / table_head_st /pad_plane_h
      RECORD / tpg_pad_plane_st /pad_plane(*)

      real    a, b, row, det
      integer isect

C     Description: Returns parametrisation(ax+by=1) for a row
C     in the isect sector
C     Input arguments:
C     isect - sector number
C     row   - row number
C     Returns:
C     a,b - row parameterisation

      real    row_cent(45),plocal(3),pglobal(3), xlocal(3), xglobal(3)
      integer flag, iret, tgc_local_to_global_p, tgc_local_to_global
      integer i,irow
      save

      data flag /1/
*
      if(flag.eq.1) then
        do i=1,45
          row_cent(i)=pad_plane(1).rad(i)
        enddo
        plocal(1)=1.0
        plocal(2)=0.0
        plocal(3)=0.0
        flag=0
      endif
      irow=int(row+0.5)
      iret=tgc_local_to_global_p(isect,plocal,pglobal)
      xlocal(2)=row_cent(irow)
      xlocal(1)=0.0
      xlocal(3)=10.0
      iret=tgc_local_to_global(isect,xlocal,xglobal)
      det = pglobal(1)*xglobal(2)-pglobal(2)*xglobal(1) 
      if(abs(det).gt.0.000001) then
          a=-pglobal(2)/det
          b= pglobal(1)/det
          tgc_row_par = STAFCV_OK
      else
          tgc_row_par = STAFCV_BAD
      endif
      end
