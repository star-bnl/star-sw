*
* $Id: rmset.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rmset.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMSET(M,N,S,Z,Z12,Z21)
          REAL                S, Z(*), Z12(*), Z21(*)
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "rzij.inc"
#include "mset.inc"
