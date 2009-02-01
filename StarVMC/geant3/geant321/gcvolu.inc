*
* $Id: gcvolu.inc,v 1.1.1.2 2009/02/01 17:10:21 fisyak Exp $
*
* $Log: gcvolu.inc,v $
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
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCVOLU_INC
#define CERNLIB_GEANT321_GCVOLU_INC
*
*
* gcvolu.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtvolu.inc"
#endif
#endif
*
* gcvolu.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcvolu
#include "geant321/incend.inc"
#endif

#endif
