*
* $Id: llong.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: llong.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:15  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:19  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION LLONG(LOC,NUM,B,IND)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER LLONG,LOC,NUM,B,IND
      INTEGER I/0/,J/0/,K/0/,L/0/,N/0/
      IF(O(68).NE.0)GOTO 99999
      N=IABS(NUM)
      L=LOC
      I=N/B
      J=1
      IF (IND.EQ.0) THEN
         L=L+1
         O(L)=O(76)
      END IF
20    IF(I.LE.0)GOTO21
         I=I/B
         J=J+1
      GOTO 20
21    CONTINUE
CCOUNT DIGITS IN EXPANSION
      IF (NUM.LT.0) THEN
         L=L+1
         O(L)=42
      END IF
      LLONG=L-LOC+J
30    CONTINUE
         K=N/B
         O(L+J)=N-K*B
         N=K
         J=J-1
         IF(J.LE.0)GOTO31
      GOTO 30
31    CONTINUE
99999 RETURN
      END
