+DECK,gfrotm.
*CMZ :          20/10/98  17.20.21  by  Pavel Nevski
*-- Author :
      SUBROUTINE GFROTM(NMAT,THETA1,PHI1,THETA2,PHI2,THETA3,PHI3)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Return ROTATION MATRICE parameters                       *
C.    *                                                                *
C.    ******************************************************************
C.
+CDE,GCBANK
C.
      IF (NMAT.LE.0)  go TO 999
      IF (JROTM.LE.0) GO TO 999
      IF (NMAT.GT.IQ(JROTM-2)) GO TO 999
 
      JR=LQ(JROTM-NMAT)
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
 
 
 
 
