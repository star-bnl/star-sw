*
* $Id: gcvdma.inc,v 1.1.1.3 2009/02/18 20:33:09 fisyak Exp $
*
* $Log: gcvdma.inc,v $
* Revision 1.1.1.3  2009/02/18 20:33:09  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:36  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCVDMA_INC
#define CERNLIB_GEANT321_GCVDMA_INC
*
*
* gcvdma.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtvdma.inc"
#endif
C
#endif

#endif
