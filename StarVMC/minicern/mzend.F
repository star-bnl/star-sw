*
* $Id: mzend.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: mzend.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:49  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:23  fca
* AliRoot sources
*
* Revision 1.2  1996/04/18 16:11:27  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:17  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE MZEND

C-    Memory manager termination,  user called

#include "zebra/zbcd.inc"
#include "zebra/zmach.inc"
#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mqsys.inc"
C--------------    End CDE                             --------------

#include "zebra/q_shiftl.inc"

      NSTORS = NQSTOR + 1
      IF (NQSTOR.LT.0)       RETURN
      IF (NQSTOR.GE.16)      RETURN

      WRITE (IQLOG,9001) NSTORS
 9001 FORMAT (1X/' MZEND.   Usage statistics for',I3,' dynamic stores.')

      JSTO = 0
   21 JDIV = ISHFTL (JSTO,26)

      CALL MZSDIV (JDIV,-7)
      CALL MZMAPS

      JSTO = JSTO + 1
      IF (JSTO.LE.NQSTOR)          GO TO 21
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
