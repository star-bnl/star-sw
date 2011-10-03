*
* $Id: exprnf.F,v 1.1.1.1 2004/01/15 00:12:19 potekhin Exp $
*
* $Log: exprnf.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:19  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:56  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:54  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.48  by  S.Giani
*-- Author :
      FUNCTION EXPRNF(A)
C
#include "geant321/crandm.inc"
C
      REAL I
C
      I = 0.0
   10 X = RANDC(ISEED)
      Z = X
   20 Y = RANDC(ISEED)
      IF(Z-Y) 50 ,50 ,30
   30 Z = RANDC(ISEED)
      IF(Z-Y) 20 ,40 ,40
   40 I = I + 1.0
      GO TO 10
   50 EXPRNF = X + I
      RETURN
      END
