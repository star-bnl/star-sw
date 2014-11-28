*
* $Id: rmscl.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rmscl.F,v $
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
          SUBROUTINE          RMSCL(M,N,S,X,X12,X21,Z,Z12,Z21)
          REAL                S, X(*),X12(*),X21(*), Z(*),Z12(*),Z21(*)
          REAL                FUNCT, A
          FUNCT(A)  =  S*A
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "rxij.inc"
#include "rzij.inc"
#include "mcpy.inc"
