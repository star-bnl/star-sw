*
* $Id: gc10ev.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gc10ev.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GC10EV_INC
#define CERNLIB_GEANT321_GC10EV_INC
*
*
* gc10ev.inc
*
#if !defined(CERNLIB_INCLUDE)
      REAL G10EV,TENEV
      PARAMETER (G10EV=1.0E-8)
      PARAMETER (TENEV=1.E-2)
C
#endif
*
* gc10ev.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gc10ev
#include "geant321/incend.inc"
#endif

#endif
