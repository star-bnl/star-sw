*
* $Id: mesage.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: mesage.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:15  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:26  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:45  cjo
* Importing agetof initial version
*
C-----------------------------------------------------------------------
      SUBROUTINE MESAGE (LEVEL,NO,K1,K2)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER LEVEL,NO,K1,K2,STATUS,MUSER
      INTEGER LLIS,DDEB
      COMMON /MTRAC/ LLIS,DDEB
      REAL ETIME,TOT,TT(2)
C PLN 01.12.95  <VAX>; +SELF,IF=VAX.;
C
CEXTERNAL PARSER$ERROR, PARSER$NORMAL;
C
C+SELF. <ENDVAX>;
      IF (LEVEL.EQ.14 .OR. LEVEL.EQ.15) O(17)=O(17)+1
      CALLFIND(LEVEL,NO,K1)
      IF(K2.NE.0)CALLMACTRC(4,K1,K2)
      IF (LEVEL.EQ.13) THEN
         CALLFIND(13,2,O(60))
         CALLFIND(13,3,O(61))
      END IF
      IF (LEVEL .LE.14) THEN
         IF(O(17).LT.100)RETURN
         CALLFIND(15,10,0)
      END IF
      IF (O(55).EQ.O(86).OR. O(55).EQ.O(87)) THEN
         O(17)=O(17)+1
         IF(O(55).EQ.O(86)) CALLFIND(16,5,0)
         IF(O(55).EQ.O(87)) CALLFIND(16,4,0)
      END IF
      IF (O(47).GT.3775.OR.O(48).GT.3825) O(17)=O(17)+1
      IF (O(48).GT.3825) THEN
         CALLFIND(16,3,O(48)-3825)
      END IF
20    IF(O(47).LE.3775)GOTO21
         CALLFIND(16,2,O(O(47)))
         O(47)=O(47)-1
      GOTO 20
21    CONTINUE
      CALLFIND(13,3,O(61))
      STATUS=MUSER('EXIT','    ',0)
      IF (STATUS.NE.1) THEN
         O(17)=O(17)+1
      END IF
      IF (O(17).EQ.0) THEN
         CALLFIND(16,6,0)
      ELSE
         CALLFIND(16,7,O(17))
C    <VAX> +SELF,IF=VAX.; cLIB$STOP(PARSER$ERROR); +SELF. <ENDVAX>
         STOP 'parser error(s) occured'
      END IF
C  IF(MODE==0) cINITAL(0,0,-1,0,0,0,0);   - PLN - 20-03-93 - binary not
Cused
C  PLN 01.12.95  <VAX>; +SELF,IF=VAX.; cMSTOP;  +SELF. <ENDVAX>;
      TOT=ETIME(TT)
      IF (DDEB.GT.0) PRINT *,' elapsed time = ',TOT,TT
      STOP
      END
