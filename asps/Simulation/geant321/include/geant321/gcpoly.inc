*
* $Id: gcpoly.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcpoly.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCPOLY_INC
#define CERNLIB_GEANT321_GCPOLY_INC
*
*
* gcpoly.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCPOLY/IZSEC,IPSEC
      INTEGER IZSEC,IPSEC
C
#endif
*
* gcpoly.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcpoly
#include "geant321/incend.inc"
#endif

#endif
