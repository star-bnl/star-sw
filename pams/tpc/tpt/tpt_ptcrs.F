* $Id: tpt_ptcrs.F,v 1.2 1998/01/27 00:35:51 fisyak Exp $
* $Log: tpt_ptcrs.F,v $
* Revision 1.2  1998/01/27 00:35:51  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_PTCRS(as,bs,an,bn,xp1,yp1)
C
C TPT_PTCRS - calculates crossing point between a row and a track
C
C DESCRIPTION:
C Routine solves a set of two linear equation. One equation parametrizes
C TPC row , the other is a linear extrapolation of the track segment
C The track is parametrized as an*x+bn*y=1
C The row is parametrized as as*x+bs*y=1
C
C INPUT ARGUMENTS:
C  as   - x coefficient for the row
C  bs   - y coefficient for the row
C  an   - x coefficient for the track
C  bn   - y coefficient for the track
C
C OUTPUT ARGUMENTS:
C  xp1 - x coordinate for the prediction
C  yp1 - y coordinate of the prediction
C
C AUTHOR:
C Idea by Bill Love.
C Written and maintained by Iwona Sakrejda
C<--------------------------------------------------------------------

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
