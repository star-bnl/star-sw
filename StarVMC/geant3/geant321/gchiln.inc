*
* $Id: gchiln.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gchiln.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:12  fisyak
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
#ifndef CERNLIB_GEANT321_GCHILN_INC
#define CERNLIB_GEANT321_GCHILN_INC
*
*
* gchiln.inc
*
#if !defined(CERNLIB_INCLUDE)
#if defined(CERNLIB_TYPE)
#include "geant321/gthiln.inc"
#endif
      COMMON/GCHILN/LARECG(2), JCGOBJ, JCGCOL, JCOUNT, JCLIPS,
     +              IMPOIN, IMCOUN, JSIX, JSIY, JSIZ,
     +              JPXC, JPYC, JPZC, ICLIP1, ICLIP2
*
#endif
*
* gchiln.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gchiln
#include "geant321/incend.inc"
#endif

#endif
