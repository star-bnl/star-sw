*
* $Id: frhonc.F,v 1.1.1.1 2004/01/15 00:12:22 potekhin Exp $
*
* $Log: frhonc.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:22  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:31:02  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:22:01  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.46  by  S.Giani
*-- Author :
*$ CREATE FRHONC.FOR
*COPY FRHONC
*
*=== frhonc ===========================================================*
*
      FUNCTION FRHONC (R)
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
*
#include "geant321/nucgeo.inc"
*
#include "geant321/nucstf.inc"
*
      IF ( R .LE. RADIU0 ) THEN
         FRHONC = RHCORE (R)
      ELSE IF ( R .LE. RADIU1 ) THEN
         FRHONC = RHSKIN (R)
      ELSE IF ( R .LE. RADTOT ) THEN
         FRHONC = RHHALO (R)
      ELSE
         FRHONC = 0.D+00
      END IF
      RETURN
      END
