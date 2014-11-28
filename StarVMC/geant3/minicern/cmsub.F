*
* $Id: cmsub.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: cmsub.F,v $
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
          SUBROUTINE          CMSUB(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          COMPLEX             X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          COMPLEX             Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A-B
#include "cxij.inc"
#include "cyij.inc"
#include "czij.inc"
#include "madd.inc"
