*
* $Id: fltrnf.F,v 1.1.1.1 2004/01/15 00:12:21 potekhin Exp $
*
* $Log: fltrnf.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:21  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:57  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:56  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 23/02/95  14.46.01  by  S.Giani
*-- Author :
C*********************************************************************
      FUNCTION FLTRNF(X)
C*********************************************************************
#include "geant321/crandm.inc"
      FLTRNF = RANDC(ISEED)
      RETURN
      END
