*
* $Id: gcpixe.inc,v 1.1.1.1 2004/07/17 20:02:49 perev Exp $
*
* $Log: gcpixe.inc,v $
* Revision 1.1.1.1  2004/07/17 20:02:49  perev
* STAR version of Geant321 TGeant3 etc
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
* Revision 1.1.1.1  1995/10/24 10:20:36  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCPIXE_INC
#define CERNLIB_GEANT321_GCPIXE_INC
*
*
* gcpixe.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCPIXE/LIMPRE,IFLAPE,ICOLOR,IXXX,IYYY,
     +ISSEEN,ISCOLO,ISLSTY,ISLWID,ISFILL,
     +IMAP,JON,NMAP,UUU,VVV,ZUV,ZNMAP1
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtpixe.inc"
#endif
 
#endif

#endif
