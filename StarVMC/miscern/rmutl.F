*
* $Id: rmutl.F,v 1.1 2005/05/26 22:28:17 fisyak Exp $
*
* $Log: rmutl.F,v $
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMUTL(N,X,X12,X21)
          REAL                X(*), X12(*), X21(*)
          IF(N .LE. 1)  RETURN
#include "rxij.inc"
#include "mutl.inc"
