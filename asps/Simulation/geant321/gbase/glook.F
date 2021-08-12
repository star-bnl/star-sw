*
* $Id: glook.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: glook.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:26  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:10  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GLOOK(NAME,IVECT,N,ILOOK)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Search position ILOOK of element NAME in array IVECT     *
C.    *       of length N                                              *
C.    *                                                                *
C.    *    ==>Called by : many GEANT and GEANG routines                *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION IVECT(1)
      CHARACTER*(*) NAME
C.
C.    ------------------------------------------------------------------
C.
      CALL UCTOH(NAME,NAMEH,4,4)
      ILOOK=IUCOMP(NAMEH,IVECT,N)
C
      END
