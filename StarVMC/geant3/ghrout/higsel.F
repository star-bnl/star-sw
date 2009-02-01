*
* $Id: higsel.F,v 1.1.1.2 2009/02/01 17:10:29 fisyak Exp $
*
* $Log: higsel.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:29  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:23  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:37  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:19  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.40  by  S.Giani
*-- Author :
      SUBROUTINE HIGSEL(ISEL)
C
#include "geant321/s_result.inc"
C
      DIMENSION RNDM(1)
      CALL GRNDM(RNDM,1)
      ISEL=1
      IF(P.LT.25.+RNDM(1)*25.) ISEL=0
      RETURN
      END
