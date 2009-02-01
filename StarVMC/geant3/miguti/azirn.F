*
* $Id: azirn.F,v 1.1.1.2 2009/02/01 17:10:34 fisyak Exp $
*
* $Log: azirn.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:34  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:57  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:57  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:43  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:53  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.48  by  S.Giani
*-- Author :
      SUBROUTINE AZIRN(SIN,COS)
#include "geant321/crandm.inc"
C       THIS ROUTINE SELECTS THE AZIMUTHAL ANGLE UNIFORMLY IN THETA
   10 R1 = SFLRAF(DUM)
      R1SQ = R1 * R1
      R2 = RANDC(ISEED)
      R2SQ = R2 * R2
      RSQ = R1SQ + R2SQ
      IF(1.0-RSQ) 10 ,20 ,20
   20 SIN = 2.0 * R1 * R2 / RSQ
      COS = (R2SQ-R1SQ) / RSQ
      RETURN
      END
