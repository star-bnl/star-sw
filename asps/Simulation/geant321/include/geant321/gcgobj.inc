*
* $Id: gcgobj.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcgobj.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCGOBJ_INC
#define CERNLIB_GEANT321_GCGOBJ_INC
*
*
* gcgobj.inc
*
#if !defined(CERNLIB_INCLUDE)
      PARAMETER (NTRCG=1)
      PARAMETER (NWB=207,NWREV=100,NWS=1500)
      PARAMETER (C2TOC1=7.7, C3TOC1=2.,TVLIM=1296.)
      COMMON /GCGOBJ/IST,IFCG,ILCG,NTCUR,NFILT,NTNEX,KCGST
     +             ,NCGVOL,IVFUN,IVCLOS,IFACST,NCLAS1,NCLAS2,NCLAS3
      COMMON /CGBLIM/IHOLE,CGXMIN,CGXMAX,CGYMIN,CGYMAX,CGZMIN,CGZMAX
C
#endif
*
* gcgobj.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcgobj
#include "geant321/incend.inc"
#endif

#endif
