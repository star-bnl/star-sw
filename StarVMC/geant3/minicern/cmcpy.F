*
* $Id: cmcpy.F,v 1.1 2009/02/25 00:08:24 fisyak Exp $
*
* $Log: cmcpy.F,v $
* Revision 1.1  2009/02/25 00:08:24  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:10  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:00  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMCPY(M,N,X,X12,X21,Z,Z12,Z21)
          COMPLEX             X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          COMPLEX             FUNCT, A
          FUNCT(A)  =  A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cxij.inc"
#include "czij.inc"
#include "mcpy.inc"
