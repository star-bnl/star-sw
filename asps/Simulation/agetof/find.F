*
* $Id: find.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: find.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:13  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:05  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C-----------------------------------------------------------------------
      SUBROUTINE FIND(L,N,V)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER L,N,V,NERD,LLONG
      INTEGER A/0/,C/0/,K/0/,Q/0/,R/0/,S/0/
      A=255
      C=A+9
      Q=1945+L
20    IF (O(Q).NE.0) THEN
         Q=O(Q)
         IF(O(Q+7).NE.N)GOTO20
         R=O(Q+2)+6
         S=O(O(Q+2)+3)
30       IF(R.GE.S)GOTO31
            C=C+1
            O(C)=O(R)
            R=R+1
         GOTO 30
31       CONTINUE
      ELSE
         C=C+LLONG(C,N,10,0)
      END IF
      IF(L.EQ.15) CALLDUMDUM(255,47,45,45,45,15,10,29,10,21,47)
      IF(L.EQ.14) CALLDUMDUM(255,47,45,32,10,27,23,18,23,16,47)
      IF (L.EQ.13.OR.L.EQ.16) THEN
         CALLDUMDUM(255,47,47,47,47,47,47,47,47,47,47)
         IF(V.NE.0) NERD=LLONG(A,V,10,0)
      END IF
      CALLRW(L,O(56),A,C)
      DO 41 K = A,C
         O(K)=O(76)
41    CONTINUE
      C=255+8
99999 RETURN
      END
