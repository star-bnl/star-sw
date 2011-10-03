*
* $Id: gcomis.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcomis.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCOMIS_INC
#define CERNLIB_GEANT321_GCOMIS_INC
*
*
* gcomis.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCOMIS/JUINIT,JUGEOM,JUKINE,JUSTEP,JUOUT,JULAST
      DIMENSION JPCOMS(6)
      EQUIVALENCE (JPCOMS,JUINIT)
*
#endif
*
* gcomis.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcomis
#include "geant321/incend.inc"
#endif

#endif
