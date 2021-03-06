*
* $Id: ganni2.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: ganni2.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:30  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:32  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:54  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:21  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE G3ANNI2
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Generates positron annihilation at rest                  *
C.    *       (alias of G3ANNIR , directly called from G3PAIRG)        *
C.    *                                                                *
C.    *    ==>Called by : G3PAIRG,G3PAIRM                              *
C.    *       Author    M.Maire *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcphys.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcking.inc"
#include "geant321/gccuts.inc"
#include "geant321/gconsp.inc"
      DIMENSION RNDM(2)
C.
      KCASE = NAMEC(11)
      IF((IANNI.NE.1).OR.(EMASS.LE.CUTGAM)) THEN
         DESTEP = DESTEP + 2*EMASS
         GO TO 999
      ENDIF
      CALL GRNDM(RNDM,2)
      CT=-1.+2.*RNDM(1)
      ST=SQRT((1.-CT)*(1.+CT))
      PHI=TWOPI*RNDM(2)
      NGKINE=NGKINE+1
      GKIN(1,NGKINE)=EMASS*ST*COS(PHI)
      GKIN(2,NGKINE)=EMASS*ST*SIN(PHI)
      GKIN(3,NGKINE)=EMASS*CT
      GKIN(4,NGKINE)=EMASS
      GKIN(5,NGKINE)=1.
      TOFD(NGKINE)=0.
      GPOS(1,NGKINE) = VECT(1)
      GPOS(2,NGKINE) = VECT(2)
      GPOS(3,NGKINE) = VECT(3)
      NGKINE=NGKINE+1
      DO 10 J=1,3
   10 GKIN(J,NGKINE)=-GKIN(J,NGKINE-1)
      GKIN(4,NGKINE)=EMASS
      GKIN(5,NGKINE)=1.
      TOFD(NGKINE)=0.
      GPOS(1,NGKINE) = VECT(1)
      GPOS(2,NGKINE) = VECT(2)
      GPOS(3,NGKINE) = VECT(3)
C
  999 END
