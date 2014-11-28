*
* $Id: mactrc.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: mactrc.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:15  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:21  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE MACTRC(W,R,S)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER W,R,S
      INTEGER K/0/,N/0/,M/0/,T/0/
      IF ( O(23).GT.0 .AND. O(29).LT.3 .AND. W.NE.4)GOTO 99999
      IF (W.EQ.2) THEN
         CALLDUMDUM(255,45,37,37,37,22,42,29,27,34,47)
         O(255+4)=(O(32)-1225)/120
         CALLCRIT(R,S)
      ELSE IF (W.EQ.4) THEN
         CALLDUMDUM(255,47,45,28,29,27,10,23,16,14,47)
         CALLCRIT(R,S)
      ELSE IF (W.EQ.5) THEN
         CALLDUMDUM(255,47,37,10,16,10,18,23,28,29,47)
         CALLCRIT(R,S)
      ELSE IF (W.EQ.6) THEN
         CALLDUMDUM(255,47,37,24,20,47,28,25,14,12,47)
         CALLCRIT(R,S)
      END IF
      IF (W.EQ.33) THEN
         CALLDUMDUM(255,47,47,27,14,13,30,12,14,13,47)
         IF(O(23).GT.0) O(255+1)=O(23)
         CALLCRIT(R+1,S+1)
      ELSE IF (W.EQ.15) THEN
         CALLDUMDUM(255,47,47,13,14,15,18,23,14,13,47)
         CALLCRIT(R+6,O(R+3))
         IF (R+6.LT.O(R+3)) THEN
            CALLDUMDUM(255,47,47,47,47,31,10,21,30,14,47)
            O(255+2)=O(20)
            CALLCRIT(S+6,O(S+3))
         ELSE
            CALLDUMDUM(255,47,23,30,21,21,47,31,10,21,47)
            CALLCRIT(S,S)
         END IF
      ELSE IF (W.EQ.12) THEN
         CALLDUMDUM(255,47,10,25,25,14,23,13,14,13,47)
         CALLCRIT(O(S-2*(3)-1),O(S-2*(3)-2) )
         CALLDUMDUM(255,47,47,47,47,47,47,47,29,24,47)
         CALLCRIT(O(S-2*(1)-1),O(S-2*(1)-2) )
      ELSE IF (W.EQ.13) THEN
         CALLDUMDUM(255,47,47,27,14,22,24,31,14,13,47)
         CALLCRIT(O(S-2*(1)-1),O(S-2*(1)-2) )
      ELSE IF (W.EQ.22) THEN
         CALLDUMDUM(255,47,47,22,10,29,12,17,14,13,47)
         IF (O(R+6).LT.0) GOTO 99999
         O(255+1)=(O(32)-1225)/120
         CALLCRIT(R+6,O(R+3) )
         K=O(S-1)
         IF (K.NE.0) THEN
            DO 21 M = 1,K
               CALLDUMDUM(255,47,47,47,47,10,27,16,47,47,47)
               O(255+8)=M
               T=O(S-2*(M)-1)
               N=O(S-2*(M)-2)
               IF (T.NE.N) CALLCRIT(T,N)
21          CONTINUE
         END IF
      END IF
99999 RETURN
      END
