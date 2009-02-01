*
* $Id: gsdvx.F,v 1.1.1.2 2009/02/01 17:10:26 fisyak Exp $
*
* $Log: gsdvx.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:26  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:57  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:36  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:39  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:56  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE G3SDVX (NAME,MOTHER,NDIV,IAXIS,STEP,C0,NUMED
     +                  ,NDVMX)
C.
C.    *******************************************************
C.    *                                                     *
C.    * DIVIDES MOTHER INTO 'NDIV' DIVISIONS OF SIZE 'STEP' *
C.    * ALONG AXIS 'IAXIS' STARTING AT COORDINATE 'C0'.     *
C.    *  NDIV OR STEP CAN BE SET NEGATIVE OR ZERO, IN WHICH *
C.    * CASE THEY ARE RECOMPUTED FROM THE MOTHER'S SIZE.    *
C.    *                                                     *
C.    *    ==>Called by : 'USER'                            *
C.    *    Authors   F.Bruyant, B.Stoer  ********           *
C.    *                                                     *
C.    *******************************************************
C.
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
      CHARACTER*4 NAME,MOTHER
C.    -------------------------------------------------------
*
      IF (NDIV.GT.0) THEN
         IF (STEP.GT.0.) THEN
            CALL G3SDVT2 (NAME, MOTHER, STEP, IAXIS, C0, NUMED, -NDIV)
         ELSE
            CALL G3SDVN2 (NAME, MOTHER, NDIV, IAXIS, C0, NUMED)
         ENDIF
      ELSE
         IF (STEP.GT.0.) THEN
            CALL G3SDVT2 (NAME, MOTHER, STEP, IAXIS, C0, NUMED, NDVMX)
         ELSE
            WRITE (CHMAIL, 1001)
            CALL GMAIL (0, 0)
            IEORUN = 1
         ENDIF
      ENDIF
*
 1001 FORMAT (' GSDVX : Call with NDIV and STEP .le. 0')
      END
