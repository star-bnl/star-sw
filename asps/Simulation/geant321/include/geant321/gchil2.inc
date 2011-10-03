*
* $Id: gchil2.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gchil2.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
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
