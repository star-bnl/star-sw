* $Id: tpt_rad_move.F,v 1.3 1999/09/14 00:59:17 liq Exp $
* $Log: tpt_rad_move.F,v $
* Revision 1.3  1999/09/14 00:59:17  liq
* add tpt_mxdim.inc
*
* Revision 1.2  1998/01/27 00:35:53  fisyak
* Split sources
*
      SUBROUTINE TPT_RAD_MOVE(i,j)

#include "tpt_mxdim.inc"

      INTEGER i,j
      REAL rad(0:mxtrack)
      INTEGER lpnt(0:mxtrack),ltot
      COMMON /sortr/rad,lpnt,ltot
C
      rad(j)=rad(i)
      lpnt(j)=lpnt(i)
C
      end
