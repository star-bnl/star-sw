*
* $Id: ertrio.inc,v 1.3 2012/03/13 20:15:52 fisyak Exp $
*
* $Log: ertrio.inc,v $
* Revision 1.3  2012/03/13 20:15:52  fisyak
* Merge with cern version
*
* Revision 1.2  2009/10/12 14:12:23  fisyak
* follow up with VMC modifications
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.2  1996/07/25 15:00:40  ravndal
* CMZ to CVS conversion artefact removed
*
* Revision 1.1  1996/03/06 15:32:34  mclareni
* Add geane321 includes
*
*
#ifndef CERNLIB_ERPRCDES_ERTRIO_INC
#define CERNLIB_ERPRCDES_ERTRIO_INC
*
*      Sequence for input-output
*
* ertrio.inc
*
      INTEGER          MXPRED
      PARAMETER (MXPRED = 10)
      DOUBLE PRECISION ERDTRP
      REAL             ERRIN, ERROUT, ERTRSP, ERXIN, ERXOUT, ERPIN,
     +                 ERPOUT
      INTEGER          NEPRED, INLIST, ILPRED, IEPRED
      COMMON /ERTRIO/  ERDTRP(5,5,MXPRED), ERRIN(15), ERROUT(15,MXPRED),
     +                 ERTRSP(5,5,MXPRED), ERXIN( 3), ERXOUT( 3,MXPRED),
     +                 ERPIN(3), ERPOUT(3,MXPRED), NEPRED,INLIST,ILPRED,
     +                 IEPRED(MXPRED)
      COMMON /ERTRIO1/ IERTR, IERTR1, IERTR2, NOPRNT
      INTEGER          IERTR, IERTR1, IERTR2, NOPRNT
#include "geant321/eropts.inc"
 
 
*
* ertrio.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant321/incbeg.inc"
     +    ERTRIO
#include "geant321/incend.inc"
#endif

#endif
