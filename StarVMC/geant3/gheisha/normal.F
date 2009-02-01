*
* $Id: normal.F,v 1.1.1.2 2009/02/01 17:10:28 fisyak Exp $
*
* $Log: normal.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:28  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:09  fisyak
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
* Revision 1.1.1.1  1999/05/18 15:55:18  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:58  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.38  by  S.Giani
*-- Author :
      SUBROUTINE NORMAL(RAN)
C
C *** NVE 14-APR-1988 CERN GENEVA ***
C
C ORIGIN : H.FESEFELDT (27-OCT-1983)
C
      DIMENSION RNDM(12)
      RAN=-6.
      CALL GRNDM(RNDM,12)
      DO 1 I=1,12
      RAN=RAN+RNDM(I)
 1    CONTINUE
      RETURN
      END
