*
* $Id: ludat3.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: ludat3.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:25  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:34  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_LUDAT3_INC
#define CERNLIB_GEANT321_LUDAT3_INC
*
*
* ludat3.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER MDCY,MDME,KFDP
      REAL    BRAT
      SAVE /LUDAT3/
*
#endif
*
* ludat3.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +    ludat3
#include "geant321/incend.inc"
#endif

#endif
