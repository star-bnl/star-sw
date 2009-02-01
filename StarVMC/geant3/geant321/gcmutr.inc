*
* $Id: gcmutr.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gcmutr.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:14  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:31  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCMUTR_INC
#define CERNLIB_GEANT321_GCMUTR_INC
*
*
* gcmutr.inc
*
#if !defined(CERNLIB_INCLUDE)
#if defined(CERNLIB_TYPE)
#include "geant321/gtmutr.inc"
#endif
*
      PARAMETER (MULTRA=50)
      CHARACTER*4 GNASH, GNNVV, GNVNV
      COMMON/GCMUTR/NCVOLS,KSHIFT,NSHIFT,ICUBE,NAIN,JJJ,
     +              NIET,IOLDSU,IVOOLD,IWPOIN,IHPOIN,IVECVO(100),
     +              PORGX,PORGY,PORGZ,POX(15),POY(15),POZ(15),GBOOM,
     +              PORMIR(18),PORMAR(18),IPORNT,
     +              ICGP,CLIPMI(6),CLIPMA(6),
     +              ABCD(4),BMIN(6),BMAX(6),CGB(16000),CGB1(16000),
     +              GXMIN(MULTRA),GXMAX(MULTRA),GYMIN(MULTRA),
     +              GYMAX(MULTRA),GZMIN(MULTRA),GZMAX(MULTRA),
     +              GXXXX(MULTRA),GYYYY(MULTRA),GZZZZ(MULTRA)
*
      COMMON/GCMUTC/   GNASH(MULTRA),GNNVV(MULTRA),GNVNV(MULTRA)
*
#endif
*
* gcmutr.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcmutr
#include "geant321/incend.inc"
#endif

#endif
