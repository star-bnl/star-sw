*
* $Id: nib.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: nib.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:16  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:29  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:45  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION NIB(L,B)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER NIB,L,B,M,N
      M=1
      N=0
20    IF(O(L).NE.O(76))GOTO21
         L=L-1
      GOTO 20
21    CONTINUE
30    IF(0.GT.O(L).OR.O(L).GE.B)GOTO31
         N=N+O(L)*M
         M=M*B
         L=L-1
      GOTO 30
31    CONTINUE
      IF (O(L).EQ.42) THEN
         L=L-1
         N=-N
      END IF
      NIB=N
99999 RETURN
      END
