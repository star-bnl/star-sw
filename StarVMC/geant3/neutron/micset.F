* $Id: micset.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE MICSET(MATNO,NKEY)
C***********************************************************************
C set a option in MICAP
C
C INPUT:   MATNO  - GEANT material number
C          NKEY   - 0 -> use single isotopes instead of the
C                                 natural composition in material MATNO
C                   1 -> use natural composition
C
C************************************************************************
C
#include "geant321/mmicap.inc"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
         FIRST = .FALSE.
         NWW = 100
         CALL CHKZEB(NWW,IXCONS)
         CALL MZLINK(IXCONS,'MICPAR',LMIST,LMIST,LMIST)
         CALL MZBOOK(IXCONS,LMIST,0,2,'MIST',0,0,NWW,0,0)
      ENDIF
 10   CONTINUE
      DO 20 I=1,IQ(LMIST-1),2
         IF(IQ(LMIST+I).EQ.MATNO) THEN
            IQ(LMIST+I+1) = NKEY
            GOTO 999
         ENDIF
         IF(IQ(LMIST+I).EQ.0) THEN
            IQ(LMIST+I) = MATNO
            IQ(LMIST+I+1) = NKEY
            GOTO 999
         ENDIF
 20   CONTINUE
C
C  Bank got to small, increase the size
      NWW = 100 + IQ(LMIST-1)
      CALL CHKZEB(NWW,IXCONS)
      CALL MZPUSH(IXCONS,LMIST,0,100,'I')
      GOTO 10
999   RETURN
      END
