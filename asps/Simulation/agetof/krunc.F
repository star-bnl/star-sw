*
* $Id: krunc.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: krunc.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:14  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:10  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE KRUNC(R)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER R
      INTEGER C/0/,D/0/,I/0/,M/0/,N/0/,P/0/,S/0/,RT/0/,FRE/0/
      IF (O(35).NE.0) THEN
         M=O(35)
20       IF(M.EQ.O(34))GOTO21
            IF (O(M+2).GE.0) THEN
               IF(O(M).NE.0)O((O(M))+1)=O(M+1)
               O(O(M+1))=O(M)
            END IF
            O(M+2)=O(90)
            M=O(M+3)
         GOTO 20
21       CONTINUE
      END IF
      N=4175
      M=O(N+3)
      D=0
      O(O(34))=0
30    IF(N.EQ.O(34))GOTO31
         M=N
         N= O(M+3)
C    P=predecessor S=successor C=concatenation
         P=O(M+1)
         S=O(M)
         C=O(M+2)
         IF (C.EQ.O(90)) THEN
            D=D+ N-M
            GOTO30
         END IF
         IF (S.GT.0) O(S+1)=O(S+1)-D
         IF (C.GT.0) O(N+1)=O(N+1)-D
         O(P)=O(P)-D
         O(M+3)=O(M+3)-D
C   MOVE IT
         DO 41 I = M,N
            O(I-D)=O(I)
41       CONTINUE
      GOTO 30
31    CONTINUE
      FRE=O(34)
      O(34)=O(34)-D
      O(O(34))=0
      IF (R.GT.D)CALLMESAGE(15, 6, 0,0)
      O(60)=376042-O(34)
      O(61)=(100*(376042-FRE))/(376042-4175)
CPN: control space in macro buffer here ?
      IF(R.EQ.0)CALLMESAGE(13,1,D,0)
      RT=100*(376042-FRE)/(376042-4175)
      IF (RT .LE. 5) PRINT *,' Parser WARNING: only ',RT,'% of buffer le
     *ft, you may run in a problem '
99999 RETURN
      END
