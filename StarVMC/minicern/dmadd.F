*
* $Id: dmadd.F,v 1.1 2005/06/09 00:05:13 fisyak Exp $
*
* $Log: dmadd.F,v $
* Revision 1.1  2005/06/09 00:05:13  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMADD(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A+B
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "dxij.inc"
#include "dyij.inc"
#include "dzij.inc"
#include "madd.inc"
