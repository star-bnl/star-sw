*
* $Id: dsfinv.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dsfinv.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DSFINV(N,A,IDIM)
          DOUBLE PRECISION    A(IDIM,*),ZERO,     X, Y
          DOUBLE PRECISION    S31, S32, S33, DOTF
          CHARACTER*6         HNAME
          DOTF(X,Y,S31)  =  X*Y + S31
          DATA      HNAME               /  'DSFINV'  /
          DATA      ZERO      /  0.D0  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
#include "sfinv.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
