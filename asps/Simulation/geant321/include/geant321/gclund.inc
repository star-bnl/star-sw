*
* $Id: gclund.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gclund.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCLUND_INC
#define CERNLIB_GEANT321_GCLUND_INC
*
*
* gclund.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCLUND/IFLUND,ECLUND
      INTEGER IFLUND
      REAL ECLUND
C
#endif
*
* gclund.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gclund
#include "geant321/incend.inc"
#endif

#endif
