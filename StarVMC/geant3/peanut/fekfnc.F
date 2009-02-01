*
* $Id: fekfnc.F,v 1.1.1.2 2009/02/01 17:10:35 fisyak Exp $
*
* $Log: fekfnc.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:35  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:49:06  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:58  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:43  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:22  fca
* AliRoot sources
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
