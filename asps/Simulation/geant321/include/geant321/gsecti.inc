*
* $Id: gsecti.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gsecti.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:50  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GSECTI_INC
#define CERNLIB_GEANT321_GSECTI_INC
*
*
* gsecti.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      INTEGER K0FLAG
      REAL AIEL,AIIN,AIFI,AICA,ALAM
C
#endif
*
* gsecti.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gsecti
#include "geant321/incend.inc"
#endif

#endif
