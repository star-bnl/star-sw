*
* $Id: gcasho.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcasho.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
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
