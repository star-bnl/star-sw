*
* $Id: rrscl.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rrscl.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RRSCL(M,N,D,D2,X,X12,X21,Z,Z12,Z21)
          REAL          D(*),D2(*),X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rdi.inc"
#include "rxij.inc"
#include "rzij.inc"
#include "rscl.inc"
