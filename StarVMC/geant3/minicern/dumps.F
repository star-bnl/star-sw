*
* $Id: dumps.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dumps.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DUMPS(N,U,U12,U22,Y,Y2,Z,Z2)
          DOUBLE PRECISION    U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, F, SIGNF
          F(A,B,SUM)  =  A*B + SUM
          SIGNF(A)  =  -A
          IF(N .LE. 0)  RETURN
#include "dujk.inc"
#include "dyj.inc"
#include "dzi.inc"
#include "umpa.inc"
