*
* $Id: gcrz.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcrz.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCRZ_INC
#define CERNLIB_GEANT321_GCRZ_INC
*
*
* gcrz.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCRZ1/NRECRZ,NRGET,NRSAVE,LRGET(20),LRSAVE(20)
      INTEGER      NRECRZ,NRGET,NRSAVE,LRGET    ,LRSAVE
      COMMON/GCRZ2/RZTAGS
      CHARACTER*8 RZTAGS(4)
C
#endif
*
* gcrz.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcrz
#include "geant321/incend.inc"
#endif

#endif
