*
* $Id: doit.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: doit.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:11  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:02  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C----------------------------------------------------------------
      SUBROUTINE DOIT(A,Z)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER A,Z
      INTEGER P/0/,T/0/,X/0/,S(900)/900*0/
      X=0
      T=Z
20    IF(T.LT.A)GOTO21
         IF (O(T).EQ.O(95)) THEN
            IF (O(T-1).EQ.O(94)) THEN
               S(X+1)=T+1
               S(X+2)=O(T)
               X=X+2
               T=T-1
            ELSE
               S(X+1)=T+1
               X=X+1
30             IF(O(T).EQ.O(107).AND.O(T+1).EQ.25)GOTO31
                  T=T-1
                  IF(T.LT.A)GOTO 99999
               GOTO 30
31             CONTINUE
               P=T+6
               O(P-1)= S(X)-P
               X=X-1
               IF (S(X).EQ.O(95)) THEN
                  O(P-4)= S(X-2)-P
                  O(P-3)= S(X-1)-P
                  O(P-2)= O(P-3)-2
                  X=X-3
               ELSE
                  O(P-2)=S(X)-P
                  O(P-3)=S(X)-P
                  O(P-4)=S(X)-P
                  X=X-1
               END IF
            END IF
         ELSE IF (O(T).EQ.O(94)) THEN
            S(X+1)= T
            X=X+1
         END IF
         T=T-1
      GOTO 20
21    CONTINUE
99999 RETURN
      END
