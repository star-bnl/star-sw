c:Copyright 1995, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:         eml_kuvec.F
C:DESCRIPTION:  Functions for handling KUIP vectors
c:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
c:AUTHOR:       mc - MCronqvist@lbl.gov- bug fix
C:BUGS:         18nov96-v001d-mc - must get vector pointer every time
C:BUGS:         04dec95-v000a-cet- needs to be a count down stack
C:HISTORY:      18nov96-v001d-mc - fixed bug in vector pointer handling
C:HISTORY:	18mar96-v001c-cet- change vector funcs. to 1 arg. APIs
C:HISTORY:	18mar96-v001b-cet- change RESULT to 1-dim vector
C:HISTORY:	18mar96-v001a-cet- change vectors to FIFO
C:HISTORY:      01dec95-v000a-cet- creation
c:>---------------------------------------------------------------------

c:<---------------------------------------------------------------------
C:ROUTINE:      SUBROUTINE EML_KUVEC_INIT()
C:DESCRIPTION:  Initialize EML KUIP vector handling
C:ARGUMENTS:    -- NONE --
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      SUBROUTINE EML_KUVEC_INIT()
      IMPLICIT NONE

#include "emlLib.inc"

      INTEGER*4 LENGTH(3), ILO, IHI

C- Vector of status values.
      LENGTH(1) = STAF_KUVEC_HISTORY
      LENGTH(2) = 0
      LENGTH(3) = 0
      CALL KUVCRE(STAF_STATUS_NAME,'I',LENGTH,ILO,IHI)
      STAF_STATUS_COUNT = 0
C- Vector of result values.
      LENGTH(1) = STAF_KUVEC_HISTORY
      LENGTH(2) = 0
      LENGTH(3) = 0
      CALL KUVCRE(STAF_RESULT_NAME,'R',LENGTH,ILO,IHI)
      STAF_RESULT_COUNT = 0

      RETURN
      END

c:<---------------------------------------------------------------------
C:ROUTINE:      SUBROUTINE SET_STAF_STATUS
C:DESCRIPTION:  Set the most recent value of the STAF_STATUS vector.
C:ARGUMENTS:    INTEGER*4 STATUS
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      SUBROUTINE SET_STAF_STATUS(STATUS)
      IMPLICIT NONE

#include "emlLib.inc"

      INTEGER*4 STATUS
      INTEGER*4 I,ILO,IHI

C- FIFO buffer approach. --
      CALL KUVECT(STAF_STATUS_NAME,ILO,IHI)
      STAF_STATUS_COUNT = MIN(STAF_STATUS_COUNT+1,STAF_KUVEC_HISTORY)
      DO I=STAF_STATUS_COUNT-1,1,-1
         IQ(ILO+I) = IQ(ILO+I-1)
      ENDDO
      IQ(ILO) = STATUS

      END
c:<---------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 FUNCTION GET_STAF_STATUS
C:DESCRIPTION:  Get Nth value of the STAF_STATUS vector.
C:ARGUMENTS:    INTEGER*4 N
C:RETURN VALUE: Nth status value
c:<---------------------------------------------------------------------
      INTEGER*4 FUNCTION GET_STAF_STATUS(N)
      IMPLICIT NONE

#include "emlLib.inc"

      INTEGER*4 N,ILO,IHI

      IF(N+1.GT.STAF_STATUS_COUNT .OR. N.LT.1) THEN
        GET_STAF_STATUS = 0
!error message here
        RETURN
      ENDIF
      CALL KUVECT(STAF_STATUS_NAME,ILO,IHI)
      GET_STAF_STATUS = IQ(ILO+N-1)

      END

c:<---------------------------------------------------------------------
C:ROUTINE:      SUBROUTINE SET_STAF_RESULT
C:DESCRIPTION:  Set the most recent value of the STAF_RESULT vector.
C:ARGUMENTS:    REAL*4 RESULT - newest value
C:RETURN VALUE: -- NONE --
c:<---------------------------------------------------------------------
      SUBROUTINE SET_STAF_RESULT(RESULT)
      IMPLICIT NONE

#include "emlLib.inc"

      REAL*4 RESULT
      INTEGER*4 I,ILO,IHI

C- FIFO buffer approach. --
      CALL KUVECT(STAF_RESULT_NAME,ILO,IHI)
      STAF_STATUS_COUNT = MIN(STAF_STATUS_COUNT+1,STAF_KUVEC_HISTORY)
      DO I=STAF_RESULT_COUNT-1,1,-1
         Q(ILO+I) = Q(ILO+I-1)
      ENDDO
      Q(ILO) = RESULT

      END

c:<---------------------------------------------------------------------
C:ROUTINE:      REAL*4 FUNCTION GET_STAF_RESULT
C:DESCRIPTION:  Get a value of the STAF_RESULT vector.
C:ARGUMENTS:    INTEGER*4 N
C:RETURN VALUE: Nth result value
c:<---------------------------------------------------------------------
      REAL*4 FUNCTION GET_STAF_RESULT(N)
      IMPLICIT NONE

#include "emlLib.inc"

      INTEGER*4 N,ILO,IHI

      IF(N.GT.STAF_KUVEC_HISTORY-1 .OR. N.LT.1) THEN
        GET_STAF_RESULT = 0
!error message here
        RETURN
      ENDIF
      CALL KUVECT(STAF_RESULT_NAME,ILO,IHI)
      GET_STAF_RESULT = Q(ILO+N-1)

      END

c:<---------------------------------------------------------------------
C:ROUTINE:      REAL*4 FUNCTION GET_STAF_RESULT
C:DESCRIPTION:  Get a value of the STAF_RESULT vector.
C:ARGUMENTS:    INTEGER*4 N
C:RETURN VALUE: Nth result value
c:<---------------------------------------------------------------------
      SUBROUTINE SET_QUEST(N)
      COMMON/QUEST/IQUEST(100)
      IQUEST(1) = N
      END
