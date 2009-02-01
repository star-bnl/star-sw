*
* $Id: trcom3.inc,v 1.1.1.2 2009/02/01 17:10:23 fisyak Exp $
*
* $Log: trcom3.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:23  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:36  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:34  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.2  1996/07/25 15:01:26  ravndal
* CMZ to CVS conversion artefact removed
*
* Revision 1.1  1996/03/06 15:32:35  mclareni
* Add geane321 includes
*
*
#ifndef CERNLIB_ERPRCDES_TRCOM3_INC
#define CERNLIB_ERPRCDES_TRCOM3_INC
*
*      Sequences of the EMC routines
*
* trcom3.inc
*
      DOUBLE PRECISION A, B, S, TN, T, COSL, SINL, COSP, SINP, COSL1
      INTEGER          NEW
      COMMON /TRCOM3/ A(5,5), B(5,5), S(15),
     +                TN(3), T(5), COSL, SINL, COSP, SINP, COSL1, NEW
*
* trcom3.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant321/incbeg.inc"
     +    TRCOM3
#include "geant321/incend.inc"
#endif

#endif
