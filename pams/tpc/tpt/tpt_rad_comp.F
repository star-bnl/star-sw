* $Id: tpt_rad_comp.F,v 1.2 1998/01/27 00:35:52 fisyak Exp $
* $Log: tpt_rad_comp.F,v $
* Revision 1.2  1998/01/27 00:35:52  fisyak
* Split sources
*
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
