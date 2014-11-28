*
* $Id: crscl.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: crscl.F,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:10  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CRSCL(M,N,D,D2,X,X12,X21,Z,Z12,Z21)
          COMPLEX       D(*),D2(*),X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cdi.inc"
#include "cxij.inc"
#include "czij.inc"
#include "rscl.inc"
