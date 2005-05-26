*
* $Id: cmutl.F,v 1.1 2005/05/26 22:28:01 fisyak Exp $
*
* $Log: cmutl.F,v $
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMUTL(N,X,X12,X21)
          COMPLEX             X(*), X12(*), X21(*)
          IF(N .LE. 1)  RETURN
#include "cxij.inc"
#include "mutl.inc"
