*
* $Id: guskip.F,v 1.1.1.1 2004/01/15 00:12:17 potekhin Exp $
*
* $Log: guskip.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:17  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:46  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      SUBROUTINE GUSKIP (ISKIP)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   User routine to skip unwanted tracks                         *
C.    *                                                                *
C.    *   Called by : GSSTAK                                           *
C.    *   Author    : F.Bruyant                                        *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
C.    ------------------------------------------------------------------
*
      ISKIP = 0
*                                                             END GUSKIP
      END
 
