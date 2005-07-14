*
* $Id: gauss.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: gauss.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
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
