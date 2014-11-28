*
* $Id: rmcpy.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rmcpy.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMCPY(M,N,X,X12,X21,Z,Z12,Z21)
          REAL                X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          REAL                FUNCT, A
          FUNCT(A)  =  A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rxij.inc"
#include "rzij.inc"
#include "mcpy.inc"
