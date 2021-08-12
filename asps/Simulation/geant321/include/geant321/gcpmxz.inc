*
* $Id: gcpmxz.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcpmxz.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCPMXZ_INC
#define CERNLIB_GEANT321_GCPMXZ_INC
*
*
* gcpmxz.inc
*
#if !defined(CERNLIB_INCLUDE)
      INTEGER MAXELZ
      PARAMETER (MAXELZ=100)
C
#endif
*
* gcpmxz.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcpmxz
#include "geant321/incend.inc"
#endif

#endif
