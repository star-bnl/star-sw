*
* $Id: cterp.F,v 1.1.1.1 2004/01/15 00:12:20 potekhin Exp $
*
* $Log: cterp.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:20  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:57  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:56  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.48  by  S.Giani
*-- Author :
      SUBROUTINE CTERP(X1,X2,X,Y1,Y2,Y)
C       THIS ROUTINE PERFORMS LINEAR INTERPOLATION
      Y=Y2-(X2-X)*(Y2-Y1)/(X2-X1)
      RETURN
      END
