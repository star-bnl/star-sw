*
* $Id: dumdum.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: dumdum.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:12  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:03  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE DUMDUM(L,A,B,C,D,E,F,G,H,I,J)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER L,A,B,C,D,E,F,G,H,I,J
      O(L)=A
      O(L+1)=B
      O(L+2)=C
      O(L+3)=D
      O(L+4)=E
      O(L+5)=F
      O(L+6)=G
      O(L+7)=H
      O(L+8)=I
      O(L+9)=J
99999 RETURN
      END
