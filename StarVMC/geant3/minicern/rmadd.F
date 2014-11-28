*
* $Id: rmadd.F,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: rmadd.F,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
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
          SUBROUTINE          RMADD(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          REAL                X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          REAL                Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A+B
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rxij.inc"
#include "ryij.inc"
#include "rzij.inc"
#include "madd.inc"
