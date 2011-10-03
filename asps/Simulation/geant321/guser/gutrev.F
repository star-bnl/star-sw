*
* $Id: gutrev.F,v 1.1.1.1 2004/01/15 00:12:18 potekhin Exp $
*
* $Log: gutrev.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:18  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:50  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:46  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      SUBROUTINE GUTREV
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to control tracking of one event            *
C.    *                                                                *
C.    *    ==>Called by : GTRIG                                        *
C.    *                                                                *
C.    ******************************************************************
C.
C.
C.    ------------------------------------------------------------------
C.
      CALL GTREVE
C
      END
