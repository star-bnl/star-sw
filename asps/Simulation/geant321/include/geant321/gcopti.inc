*
* $Id: gcopti.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcopti.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCOPTI_INC
#define CERNLIB_GEANT321_GCOPTI_INC
*
*
* gcopti.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCOPTI/ IOPTIM
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtopti.inc"
#endif
#endif
*
* gcopti.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcopti
#include "geant321/incend.inc"
#endif

#endif
