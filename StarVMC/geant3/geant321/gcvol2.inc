*
* $Id: gcvol2.inc,v 1.1.1.2 2009/02/01 17:10:21 fisyak Exp $
*
* $Log: gcvol2.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:21  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:18  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:33  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCVOL2_INC
#define CERNLIB_GEANT321_GCVOL2_INC
*
*
* gcvol2.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCVOL2/NLEVE2,NAMES2(15),NUMB2(15),
     +LVOL2(15),LIND2(15),INFRO2,NLDEV2(15),LINMX2(15),
     +GTRAN2(3,15),GRMAT2(10,15),GONLY2(15),GLX2(15)
      INTEGER NLEVE2,NAMES2,NUMB2,LVOL2,LIND2,INFRO2,NLDEV2,LINMX2
      REAL GTRAN2,GRMAT2,GONLY2,GLX2
C
#endif
*
* gcvol2.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcvol2
#include "geant321/incend.inc"
#endif

#endif
