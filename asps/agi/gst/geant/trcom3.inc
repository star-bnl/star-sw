*
* $Id: trcom3.inc,v 1.2 1998/04/16 16:59:51 fisyak Exp $
*
* $Log: trcom3.inc,v $
* Revision 1.2  1998/04/16 16:59:51  fisyak
* 2nd pass with gstar
*
*
*
* trcom3.inc
*
#if defined(CERNLIB_SGI6)
*CMZ :  1.00/00 26/08/95  00.39.30  by  Pavel Nevski
*-- Author :
      DOUBLE PRECISION A, B, S, TN, T, COSL, SINL, COSP, SINP, COSL1
      INTEGER          NEW
      COMMON /TRCOM3/ A(5,5), B(5,5), S(15),
     +                TN(3), T(5), COSL, SINL, COSP, SINP, COSL1, NEW
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
