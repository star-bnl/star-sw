c:Copyright 1995, Lawrence Berkeley National Laboratory
c:>---------------------------------------------------------------------
C:FILE:         eml_kuvec.F
C:DESCRIPTION:  Functions for handling KUIP vectors
c:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
C:BUGS:         04dec95-v000a-cet- needs to be a count down stack
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

      INTEGER*4 LENGTH(3)

C- Vector of status values.
      LENGTH(1) = STAF_KUVEC_HISTORY
      LENGTH(2) = 0
      LENGTH(3) = 0
      CALL KUVCRE('STAF_STATUS','I',LENGTH,
     +		STAF_STATUS_LLOW,STAF_STATUS_LHIGH)
      STAF_STATUS_COUNT = 0

C- Vector of result values.
      LENGTH(1) = STAF_KUVEC_HISTORY
      LENGTH(2) = 0
      LENGTH(3) = 0
      CALL KUVCRE('STAF_RESULT','R',LENGTH,
     +		STAF_RESULT_LLOW,STAF_RESULT_LHIGH)
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
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----
      INTEGER*4 I

c     write(*,*)'DEBUG:: SET_STAF_STATUS STATUS = ',STATUS

C- Circular buffer approach. --
c     STAF_STATUS_COUNT = STAF_STATUS_COUNT + 1
c     IF(STAF_STATUS_COUNT.GT.STAF_KUVEC_HISTORY)THEN
c        STAF_STATUS_COUNT = 1
c     ENDIF
c     STAF_STATUS_VECTOR(STAF_STATUS_COUNT) = STATUS

C- FIFO buffer approach. --
      STAF_STATUS_COUNT = MIN(STAF_STATUS_COUNT + 1,STAF_KUVEC_HISTORY)
      DO I=STAF_STATUS_COUNT-1,1,-1
	 STAF_STATUS_VECTOR(I+1) = STAF_STATUS_VECTOR(I)
      ENDDO
      STAF_STATUS_VECTOR(1) = STATUS
-- 23OCT96 ---- TEMPORARY DEBUG HACK --*/

      RETURN
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

      INTEGER*4 N
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----

      GET_STAF_STATUS = 
     + STAF_STATUS_VECTOR(MAX(1,MIN(N,STAF_STATUS_COUNT)))
-- 23OCT96 ---- TEMPORARY DEBUG HACK --*/

      RETURN
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
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----

      INTEGER*4 I

C- FIFO buffer approach. --
      STAF_RESULT_COUNT = MIN(STAF_RESULT_COUNT + 1,STAF_KUVEC_HISTORY)
      DO I=STAF_RESULT_COUNT-1,1,-1
         STAF_RESULT_VECTOR(I+1) = STAF_RESULT_VECTOR(I)
      ENDDO

      STAF_RESULT_VECTOR(1) = RESULT
-- 23OCT96 ---- TEMPORARY DEBUG HACK --*/

      RETURN
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

      INTEGER*4 N
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----

      GET_STAF_RESULT
     += STAF_RESULT_VECTOR(MAX(1,MIN(N,STAF_RESULT_COUNT)))
-- 23OCT96 ---- TEMPORARY DEBUG HACK --*/

      RETURN
      END

