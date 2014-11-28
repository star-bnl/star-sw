*
* $Id: dvset.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dvset.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVSET(N,S,Z,Z2)
          DOUBLE PRECISION    S, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN
#include "dzj.inc"
#include "vset.inc"
