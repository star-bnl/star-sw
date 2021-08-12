*
* $Id: cgcaan.inc,v 1.1.1.1 2004/01/15 00:12:22 potekhin Exp $
*
* $Log: cgcaan.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:22  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:44  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:42  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CGCAAN_INC
#define CERNLIB_GEANT321_CGCAAN_INC
*
*
* cgcaan.inc
*
#if !defined(CERNLIB_INCLUDE)
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION RAD,COSI,SINI,COSDA(2),SINDA(2),XYHA(4,2)
      INTEGER          NASTP(2)
      COMMON /CGCAAN/  RAD,COSI,SINI,COSDA,SINDA,XYHA,NASTP
     +                ,IFULL,ATREDG
#endif
#endif
#if !defined(CERNLIB_INCLUDE)
#if defined(CERNLIB_SINGLE)
      REAL             COSDA(2),SINDA(2),XYHA(4,2)
      INTEGER          NASTP(2)
      COMMON /CGCAAN/  RAD,COSI,SINI,COSDA,SINDA,XYHA,NASTP
     +                ,IFULL,ATREDG
#endif
#endif
*
* cgcaan.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant321/incbeg.inc"
       +   CGCAAN
#include "geant321/incend.inc"
*
#endif

#endif
