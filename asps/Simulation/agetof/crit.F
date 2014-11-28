*
* $Id: crit.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: crit.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:11  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:00  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE CRIT(A,Z)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER A,Z,SH
      INTEGER TITO(9)/9*0/,B/0/,C/0/,C1/0/
      DATA SH/35/
      B=A
20    IF(B.GE.Z)GOTO21
         C=255+9
         IF (O(255).EQ.47) THEN
            C=C+SH
         END IF
C    PLN - dense tracing
30       IF(C.GE.334.OR.B.GE.Z)GOTO31
            C=C+1
            O(C)=O(B)
            B=B+1
            IF(O(C).EQ.O(76))O(C)=37
            IF(O(C).GT.100+11) O(C)=100
            IF(O(C).EQ.-1) O(C)=O(82)
            IF(O(C).EQ.-2) O(C)=O(83)
            IF(O(C).EQ.-3 .OR.O(C).EQ.-4)O(C)=91
         GOTO 30
31       CONTINUE
         IF (O(255).EQ.47) THEN
            DO 41 C1 = 255+1,255+9
               O(C1+SH)=O(C1)
               O(C1)=TITO(C1-255)
41          CONTINUE
            CALLRW(33,O(56),255,C)
            DO 51 C1 = 255,334
               O(C1)=47
51          CONTINUE
            GOTO 99999
         ELSE
            DO 61 C1 = 255+1,255+9
               TITO(C1-255)=O(C1)
61          CONTINUE
         END IF
      GOTO 20
21    CONTINUE
99999 RETURN
      END
