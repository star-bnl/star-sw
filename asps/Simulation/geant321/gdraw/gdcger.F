*
* $Id: gdcger.F,v 1.1.1.1 2004/01/15 00:12:04 potekhin Exp $
*
* $Log: gdcger.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:04  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:38  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:20  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.26  by  S.Giani
*-- Author :
*
      SUBROUTINE GDCGER(CGERR)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    This subroutine returns Error Message in the CG-Object      *
C.    *    according to the value of CGERR.                            *
C.    *                                                                *
C.    *    Input Parameter:                                            *
C.    *                                                                *
C.    *        CGERR:  First word of the CG array                      *
C.    *                                                                *
C.    *                                                                *
C.    *    ==>Called by :   GDCGCL,GDCGOB                              *
C.    *                                                                *
C.    *       Author : J. Salt   **********                            *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcunit.inc"
#include "geant321/gcgobj.inc"
*
      IF(CGERR.LT.-0.0001)THEN
         KCGST=-2
         WRITE(CHMAIL,10000)
      ELSE
         KCGST=-3
         WRITE(CHMAIL,10100)
      ENDIF
      CALL GMAIL(0,0)
*
10000 FORMAT(' Error in CG, Lack of Memory  ')
10100 FORMAT(' Error in CG, Wrong Shape Parameters  ')
*
      END
 
