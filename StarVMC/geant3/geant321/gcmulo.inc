*
* $Id: gcmulo.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gcmulo.inc,v $
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
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCMULO_INC
#define CERNLIB_GEANT321_GCMULO_INC
*
*
* gcmulo.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
#endif
*
* gcmulo.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcmulo
#include "geant321/incend.inc"
#endif

#endif
