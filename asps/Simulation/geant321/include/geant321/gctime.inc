*
* $Id: gctime.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gctime.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCTIME_INC
#define CERNLIB_GEANT321_GCTIME_INC
*
*
* gctime.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCTIME/TIMINT,TIMEND,ITIME,IGDATE,IGTIME
      INTEGER ITIME,IGDATE,IGTIME
      REAL TIMINT,TIMEND
C
#endif
*
* gctime.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gctime
#include "geant321/incend.inc"
#endif

#endif
