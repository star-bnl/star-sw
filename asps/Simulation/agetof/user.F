*
* $Id: user.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: user.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:17  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:35  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:46  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION USER(A,Z)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER USER,A,Z,K,L,STATUS,MUSER
      CHARACTER*132 IN,OUT
      DO 21 K = A,Z
         IN(K-A+1:K-A+1)=CHAR(O(769+O(K)))
21    CONTINUE
      L=LEN(OUT)
      STATUS=MUSER(IN(2:Z-A),OUT,L)
      IF (STATUS.NE.1) THEN
         CALLMESAGE(14, 30, A,Z+1)
         Z=A
         O(A)=36
      ELSE
         Z=A+L-1
         DO 31 K = A,Z
            O(K)=O(969+ICHAR(OUT(1+K-A:)))
31       CONTINUE
      END IF
      USER=1
      PRINT *,' in user '
99999 RETURN
      END
