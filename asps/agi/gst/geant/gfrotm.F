#include "geant321/pilot.h"
*CMZ :          22/10/98  14.23.03  by  Pavel Nevski
*-- Author :    Pavel Nevski
      SUBROUTINE GFROTM(IROT,THETA1,PHI1,THETA2,PHI2,THETA3,PHI3)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Return ROTATION MATRICE parameters                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
C.
      IF (JROTM.LE.0) GO TO 999
      IF (IROT.LE.0)  go TO 999
      IF (IROT.GT.IQ(JROTM-2)) GO TO 999

      JR=LQ(JROTM-IROT)
      IF(JR.LE.0) GO TO 999

C
      THETA1 = Q(JR + 11)
      PHI1   = Q(JR + 12)
      THETA2 = Q(JR + 13)
      PHI2   = Q(JR + 14)
      THETA3 = Q(JR + 15)
      PHI3   = Q(JR + 16)
C
 999  RETURN
      END




