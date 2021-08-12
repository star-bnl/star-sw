*
* $Id: guplsh.F,v 1.1.1.1 2004/01/15 00:12:17 potekhin Exp $
*
* $Log: guplsh.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:17  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:47  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      FUNCTION GUPLSH(MED0,MED1)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *    ==>Called by : GLISUR                                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gctmed.inc"
C.
C.    ------------------------------------------------------------------
C.
*
* *** By default this defines perfect smoothness
      GUPLSH = 1.
C
      END
