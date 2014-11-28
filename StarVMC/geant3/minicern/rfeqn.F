*
* $Id: rfeqn.F,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: rfeqn.F,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:03  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RFEQN(N,A,IDIM,IR,K,B)
          INTEGER             IR(*)
          REAL                A(IDIM,*),B(IDIM,*),X,Y,TE
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                S21,      S22,      DOTF
          DOTF(X,Y,S21)  =  X*Y + S21
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    S21,      S22,      DOTF
          DOTF(X,Y,S21)  =  DBLE(X)*DBLE(Y) + S21
#endif
          DATA      HNAME               /  ' RFEQN'  /
#include "feqn.inc"
          RETURN
          END
