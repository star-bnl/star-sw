* $Id: tpt_rad_move.F,v 1.2 1998/01/27 00:35:53 fisyak Exp $
* $Log: tpt_rad_move.F,v $
* Revision 1.2  1998/01/27 00:35:53  fisyak
* Split sources
*
      SUBROUTINE TPT_RAD_MOVE(i,j)

#include "tpt_pointers.inc"

      INTEGER i,j
      REAL rad(0:mxtrack)
      INTEGER lpnt(0:mxtrack),ltot
      COMMON /sortr/rad,lpnt,ltot
C
      rad(j)=rad(i)
      lpnt(j)=lpnt(i)
C
      end
