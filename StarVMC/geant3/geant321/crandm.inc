*
* $Id: crandm.inc,v 1.1.1.3 2009/02/18 20:33:10 fisyak Exp $
*
* $Log: crandm.inc,v $
* Revision 1.1.1.3  2009/02/18 20:33:10  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:44  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CRANDM_INC
#define CERNLIB_GEANT321_CRANDM_INC
*
*
* crandm.inc
*
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION RANDC
C
#endif
#if defined(CERNLIB_SINGLE)
      REAL RANDC
C
#endif

#endif
