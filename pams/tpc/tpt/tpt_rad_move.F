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
