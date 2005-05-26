*
* $Id: rfinv.F,v 1.1 2005/05/26 22:28:17 fisyak Exp $
*
* $Log: rfinv.F,v $
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:03  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RFINV(N,A,IDIM,IR)
          INTEGER             IR(*)
          REAL                A(IDIM,*), X, Y, TI
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                ZERO, S31, S32, S33, S34, DOTF
          DOTF(X,Y,S31)  =  X*Y + S31
          DATA      ZERO      /  0.  /
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    ZERO, S31, S32, S33, S34, DOTF
          DOTF(X,Y,S31)  =  DBLE(X)*DBLE(Y) + S31
          DATA      ZERO      /  0.D0  /
#endif
          DATA      HNAME               /  ' RFINV'  /
#include "finv.inc"
          RETURN
          END
