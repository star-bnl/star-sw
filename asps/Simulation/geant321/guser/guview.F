*
* $Id: guview.F,v 1.1.1.1 2004/01/15 00:12:18 potekhin Exp $
*
* $Log: guview.F,v $
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
      SUBROUTINE GUVIEW(IDS,IVS,ICS,IVIEW)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine for interactive version                     *
C.    *                                                                *
C.    *    ==>Called by : <GXINT>, GINC1                               *
C.    *                                                                *
C.    ******************************************************************
C.
C.
C.    ------------------------------------------------------------------
C.
      CHARACTER*(*) ICS
#if defined(CERNLIB_USRJMP)
#include "geant321/gcjump.inc"
#endif
#if !defined(CERNLIB_USRJMP)
      CALL GUSTEP
#endif
#if defined(CERNLIB_USRJMP)
      CALL JUMPT0(JUSTEP)
#endif
      END
