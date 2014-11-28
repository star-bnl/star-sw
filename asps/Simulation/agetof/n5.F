*
* $Id: n5.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: n5.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:16  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:28  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:45  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION N5(L,M,K)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER N5,L,M,K,J,NIB
      J=537+4
      N5=NIB(J,10)
      IF (L.GT.N5.OR.N5.GT.M) THEN
         CALLMESAGE(14, 3, 0,0)
         N5=K
      END IF
99999 RETURN
      END
