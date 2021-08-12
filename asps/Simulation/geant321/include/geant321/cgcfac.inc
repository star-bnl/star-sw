*
* $Id: cgcfac.inc,v 1.1.1.1 2004/01/15 00:12:22 potekhin Exp $
*
* $Log: cgcfac.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:22  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:45  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:42  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CGCFAC_INC
#define CERNLIB_GEANT321_CGCFAC_INC
*
*
* cgcfac.inc
*
#if !defined(CERNLIB_INCLUDE)
      PARAMETER (LABC=1500)
      COMMON /CGCFAC/ F1(LABC),F2(LABC),F3(LABC),RFMIN(3),RFMAX(3),
     +                AABCD(4),DRFACE(5)
#endif
*
* cgcfac.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant321/incbeg.inc"
       +   CGCFAC.
#include "geant321/incend.inc"
*
#endif

#endif
