*
* $Id: dmscl.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dmscl.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:13  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMSCL(M,N,S,X,X12,X21,Z,Z12,Z21)
          DOUBLE PRECISION    S, X(*),X12(*),X21(*), Z(*),Z12(*),Z21(*)
          DOUBLE PRECISION    FUNCT, A
          FUNCT(A)  =  S*A
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "dxij.inc"
#include "dzij.inc"
#include "mcpy.inc"
