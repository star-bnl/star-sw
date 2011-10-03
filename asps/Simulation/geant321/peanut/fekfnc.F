*
* $Id: fekfnc.F,v 1.1.1.1 2004/01/15 00:12:22 potekhin Exp $
*
* $Log: fekfnc.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:22  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:31:02  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:22:00  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.46  by  S.Giani
*-- Author :
*$ CREATE FEKFNC.FOR
*COPY FEKFNC
*
*=== fekfnc ===========================================================*
*
      FUNCTION FEKFNC (PF,I)
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
*
#include "geant321/nucgeo.inc"
#include "geant321/nucdat.inc"
*
      IF ( PFRCEN (I) - PF .LE. ANGLGB ) THEN
         FEKFNC = EKFCEN (I)
      ELSE
         FEKFNC = SQRT ( AMNUSQ (I) + PF**2 ) - AMNUCL (I)
      END IF
      RETURN
      END
