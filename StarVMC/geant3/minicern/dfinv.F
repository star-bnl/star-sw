*
* $Id: dfinv.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: dfinv.F,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:13  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:03  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DFINV(N,A,IDIM,IR)
          INTEGER             IR(*)
          DOUBLE PRECISION    A(IDIM,*),ZERO,     X, Y, TI
          DOUBLE PRECISION    S31, S32, S33, S34, DOTF
          CHARACTER*6         HNAME
          DATA      HNAME               /  ' DFINV'  /
          DOTF(X,Y,S31)  =  X*Y + S31
          DATA      ZERO      /  0.D0  /
#include "finv.inc"
          RETURN
          END
