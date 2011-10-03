*
* $Id: cgctra.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: cgctra.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:45  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:42  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CGCTRA_INC
#define CERNLIB_GEANT321_CGCTRA_INC
*
*
* cgctra.inc
*
#if !defined(CERNLIB_INCLUDE)
      PARAMETER (NTMAX = 3)
      COMMON /CGCTRA/ TSCRN(4,3,NTMAX),TBACK(4,3,NTMAX)
#endif
*
* cgctra.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant321/incbeg.inc"
       +   CGCTRA.
#include "geant321/incend.inc"
*
#endif

#endif
