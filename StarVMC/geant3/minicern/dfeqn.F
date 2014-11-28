*
* $Id: dfeqn.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: dfeqn.F,v $
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
          SUBROUTINE          DFEQN(N,A,IDIM,IR,K,B)
          INTEGER             IR(*)
          DOUBLE PRECISION    A(IDIM,*),B(IDIM,*),X,Y,TE
          DOUBLE PRECISION    S21, S22, DOTF
          CHARACTER*6         HNAME
          DOTF(X,Y,S21)  =  X*Y + S21
          DATA      HNAME               /  ' DFEQN'  /
#include "feqn.inc"
          RETURN
          END
