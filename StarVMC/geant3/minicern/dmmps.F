*
* $Id: dmmps.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dmmps.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:13  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMMPS(M,N,X,X12,X21,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, F, SIGNF
          F(A,B,SUM)  =  A*B + SUM
          SIGNF(A)    =  -A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "dxij.inc"
#include "dyj.inc"
#include "dzi.inc"
#include "mmpa.inc"
