      INTEGER FUNCTION TPT_RAD_COMP(i,j)
      IMPLICIT NONE

#include "tpt_pointers.inc"

      INTEGER i,j
      REAL rad(0:mxtrack),test
      INTEGER lpnt(0:mxtrack),ltot
      COMMON /sortr/rad,lpnt,ltot
C
      test = rad(i) - rad(j)
      if(test.gt.0) then
              tpt_rad_comp = 1
      else
              tpt_rad_comp = -1
      endif
      end
