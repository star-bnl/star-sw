*
* $Id: muser.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: muser.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:16  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:27  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:45  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION MUSER(IN,OUT,L)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER MUSER,L
      CHARACTER*132 IN,OUT
C ARGUMENTS are: Input string, Output string, Output lengt=
      MUSER=1
99999 RETURN
      END
