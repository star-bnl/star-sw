*
* $Id: gcasho.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gcasho.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:11  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:29  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCASHO_INC
#define CERNLIB_GEANT321_GCASHO_INC
*
*
* gcasho.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCASHO/ZMED,AMED,DMED,E0MED,ZSMED(50),ESMED(50),ALFA,
     *             STEP,PLIN,PLOG,BE2,PLASM,TRNSMA,
     *             BOSC(50),AOSC(50),EOSC(50),ZOSC(50),EMEAN,
     *             CMGO(2000),EMGO,EMGOMI,
     *             NSMED,IOSC(50),NOSC,NMGO,NMGOMA
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtasho.inc"
#endif
#endif

#endif
