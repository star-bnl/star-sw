*
* $Id: dnorm.F,v 1.1.1.1 2004/01/15 00:12:18 potekhin Exp $
*
* $Log: dnorm.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:18  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:52  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1996/03/06 15:37:36  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE DNORM(X,AX)
*     ======================
*
*     Double Precision Vector Normalization
*
*     Author: V.Innocente (Naples)
*
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
#endif
      DIMENSION X(3)
*
      AX = SQRT(X(1)**2+X(2)**2+X(3)**2)
      AX1 = 1./AX
      X(1) = AX1*X(1)
      X(2) = AX1*X(2)
      X(3) = AX1*X(3)
*
      END
