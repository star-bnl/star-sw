*
* $Id: erstor.F,v 1.3 2009/02/01 17:29:50 fisyak Exp $
*
* $Log: erstor.F,v $
* Revision 1.3  2009/02/01 17:29:50  fisyak
* Resolve conflicts
*
* Revision 1.1.1.1  2008/12/10 20:46:28  fisyak
* Merge with macos version
*
* Revision 1.2  2005/06/09 00:10:03  fisyak
* comply with STAR
*
* Revision 1.2  2004/02/12 15:33:10  fisyak
* move STAR version of erdecks from rexe to geant321
*
* Revision 1.2  2000/01/10 16:22:14  fisyak
* Modify for multiple volume entries
*
* Revision 1.1  1999/12/07 15:44:30  fisyak
* Add geane, new TGeant3 from Alice
*
* Revision 1.1.1.1  1996/03/06 15:37:35  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE ERSTOR
*
C.    ******************************************************************
C.    *                                                                *
C.    *       Store error matrix informations                          *
C.    *                                                                *
C.    *    ==>Called by : ERTRGO , ERTRCH , ERTRNT                     *
C.    *       Author    M.Maire, E.Nagy  *********                     *
C.    *                                                                *
C.    ******************************************************************
*
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/erwork.inc"
#include "geant321/ertrio.inc"
#include "geant321/trcom3.inc"
*
      DOUBLE PRECISION  C(5,5), DUM(5,5)
*
* *** Write out results at intermediate points
*
      ILPRED = ILPRED + 1
*yf   IF (ILPRED.GE.NEPRED) ISTOP = 1
      ISTOP = 1
      IEPRED(ILPRED) = INLIST
*
* *** Leave B-matrix intact for eventual further tracking
*
      IF (.NOT. LEONLY) THEN
         DO 6 I = 1, 5
            DO 5 J = 1, 5
               C(I,J) = B(I,J)
    5       CONTINUE
    6    CONTINUE
*
* ***    Error matrix into single precision
*
         DO 10 I = 1, 15
            ERROUT(I,ILPRED) = EF(I)
   10    CONTINUE
*
* ***    If backtracking - vector, charge, error- and transport-matrix
*        in original direction
*
         IF (BACKTR.LT.0.) THEN
            CALL ERBCER (ERROUT(1,ILPRED))
            CALL ERBCTR (C)
         ENDIF
      ENDIF
*
* *** Back into SC variables
*
      IF (VECT(7) .LT. 1.E-30) VECT(7) = 1.E-30
      PF(1) = 1./VECT(7)
      PF(2) = BACKTR*ASIN (VECT(6))
      IF (ABS (VECT(4)) .LT. 1.E-30) VECT(4) = 1.E-30
      PF(3) = ATAN2 (BACKTR*VECT(5), BACKTR*VECT(4))
*
* *** Transform error matrix in the variables of the plane where
*         tracking terminates
*
      IF (LEPLAN) THEN
         CALL TRSCSD (PF, ERROUT(1,ILPRED), PF ,ERROUT(1,ILPRED), HF,
     +                CHARGE, IER ,SPU, ERPLO(1,1,INLIST),
     +                ERPLO(1,2,INLIST))
         IF (LEONLY) GOTO 25
         CALL DMM55 (A, C, DUM)
         CALL DMM55 (DUM, ASDSC, C)
      ENDIF
*
* *** Transport matrix in single precision
*
      DO 20 I = 1,5
         DO 15 J = 1,5
            ERDTRP(I,J,ILPRED) = C(I,J)
            ERTRSP(I,J,ILPRED) = C(I,J)
   15    CONTINUE
   20 CONTINUE
*
   25 CONTINUE
      DO 30 I = 1, 3
         ERXOUT(I,ILPRED) = VECT(I)
         ERPOUT(I,ILPRED) = PF(I)
   30 CONTINUE
*
      END
