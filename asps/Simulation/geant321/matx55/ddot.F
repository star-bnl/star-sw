*
* $Id: ddot.F,v 1.1.1.1 2004/01/15 00:12:18 potekhin Exp $
*
* $Log: ddot.F,v $
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
      DOUBLE PRECISION FUNCTION DDOT(X,Y)
*     ===================================
*
*     Double Precision Scalar Product
*
*     Author: V.Innocente (Naples)
*
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
#endif
      DIMENSION X(3),Y(3)
*
      DDOT = X(1)*Y(1)+X(2)*Y(2)+X(3)*Y(3)
*
      END
