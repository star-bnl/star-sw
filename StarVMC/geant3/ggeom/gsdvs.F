*
* $Id: gsdvs.F,v 1.1.1.2 2009/02/01 17:10:26 fisyak Exp $
*
* $Log: gsdvs.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:26  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:56  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:36  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:39  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:55  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE G3SDVS(CHNAME,CHMOTH,STEP,IAXIS,NUMED)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    To be replaced in the future by GSDVT                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcunit.inc"
      CHARACTER*4 CHNAME,CHMOTH
      WRITE(CHMAIL,
     +'('' *** GSDVS: Obsolete Routine -- Call GSDVT instead'')')
      CALL GMAIL(1,0)
      WRITE(CHMAIL,
     +'(''            This code will be removed from next version'')')
      CALL GMAIL(0,1)
      CALL G3SDVT(CHNAME,CHMOTH,STEP,IAXIS,NUMED,0)
      END
