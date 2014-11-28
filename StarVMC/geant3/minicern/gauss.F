*
* $Id: gauss.F,v 1.1.1.1 2009/02/18 20:32:28 fisyak Exp $
*
* $Log: gauss.F,v $
* Revision 1.1.1.1  2009/02/18 20:32:28  fisyak
*
*
* Revision 1.1.1.1  2002/06/16 15:18:45  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:34  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
C This will be GAUSS,IF=DOUBLE and GAUSS64,IF=-DOUBLE.
      FUNCTION GAUSS(F,A,B,EPS)

      CHARACTER NAME*(*)
      PARAMETER (NAME = 'GAUSS')
#include "gausscod.inc"
   99 GAUSS=H
      RETURN
      END
