      INTEGER FUNCTION TPT_PTCRS(as,bs,an,bn,xp1,yp1)

      IMPLICIT NONE
#include "PAM.inc"

      REAL as,bs,an,bn,xp1,yp1,tol,den
      PARAMETER (tol = 0.000000001)
C
      tpt_ptcrs = STAFCV_OK
      den = as*bn-bs*an
      if(abs(den).lt.tol) then
         tpt_ptcrs = STAFCV_BAD
      else
         xp1 = - (bs - bn)/den
         yp1 =   (as - an)/den
      endif
C
      end
