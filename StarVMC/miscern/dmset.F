*
* $Id: dmset.F,v 1.1 2005/05/26 22:28:02 fisyak Exp $
*
* $Log: dmset.F,v $
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMSET(M,N,S,Z,Z12,Z21)
          DOUBLE PRECISION    S, Z(*), Z12(*), Z21(*)
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "dzij.inc"
#include "mset.inc"
