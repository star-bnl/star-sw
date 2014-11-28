*
* $Id: dsfeqn.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dsfeqn.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DSFEQN(N,A,IDIM,K,B)
          DOUBLE PRECISION    A(IDIM,*),B(IDIM,*),X,Y
          DOUBLE PRECISION    S21, S22, DOTF
          CHARACTER*6         HNAME
          DATA      HNAME               /  'DSFEQN'  /
          DOTF(X,Y,S21)  =  X*Y + S21
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LE. 0)  GOTO 900
#include "sfeqn.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
