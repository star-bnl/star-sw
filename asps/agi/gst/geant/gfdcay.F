*
* $Id: gfdcay.F,v 1.2 1998/04/16 16:59:31 fisyak Exp $
*
* $Log: gfdcay.F,v $
* Revision 1.2  1998/04/16 16:59:31  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          01/07/97  10.45.18  by  Pavel Nevski
*CMZ :  1.30/00 19/04/97  17.50.46  by  Pavel Nevski
*CMZ :  3.21/03 02/08/94  18.13.20  by  S.Ravndal
*-- Author :
      SUBROUTINE GFDCAY (IPART,BRATIO,MODE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Return branching ratios and decay modes for GEANT particles.  *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    P.Nevski                                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcunit.inc"
      REAL      BRATIO(6)
      INTEGER   MODE(6)
C.
C.    ------------------------------------------------------------------
C.
      CALL VZERO(BRATIO,6)
      CALL VZERO(MODE,6)
      IF (IPART.LE.0)      RETURN
      IF (IPART.GT.NPART)  RETURN
C
C     Particle pointer.
C
      JPA = LQ(JPART-IPART)
      IF (JPA.LE.0)        RETURN
C
C PABR bank
      JDK1=LQ(JPA-1)
      IF (JDK1.LE.0)       RETURN
C PAMO bank
      JDK2=LQ(JPA-2)
      IF (JDK2.LE.0)       RETURN
C
C     COPY branching ratios & decay modes.
C
      DO I=1,6
         BRATIO(I) = Q(JDK1+I)
         MODE(I)   = IQ(JDK2+I)
      ENDDO
C
  99  RETURN
      END
 
