*
* $Id: gchil2.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gchil2.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:12  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:31  fisyak
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
#ifndef CERNLIB_GEANT321_GCHIL2_INC
#define CERNLIB_GEANT321_GCHIL2_INC
*
*
* gchil2.inc
*
#if !defined(CERNLIB_INCLUDE)
#if defined(CERNLIB_TYPE)
#include "geant321/gthil2.inc"
#endif
      COMMON/GCHIL2/LARETT(2),JTICK,JMYLL,JFIMOT,JFISCA,JFINAM,
     +              JAASS1,JAASS2,
     +              JAASS3,JAASS4,JTICKS,JMYLLS,JMYMOT
*
#endif
*
* gchil2.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gchil2
#include "geant321/incend.inc"
#endif

#endif
