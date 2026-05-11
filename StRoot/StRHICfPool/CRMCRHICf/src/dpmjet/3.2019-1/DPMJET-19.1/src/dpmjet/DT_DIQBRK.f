
      SUBROUTINE DT_DIQBRK
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , vv
      SAVE 
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C event flag
      INCLUDE 'inc/dtevno'
 
C     IF(DT_RNDM(VV).LE.0.5D0)THEN
C       CALL GSQBS1(NHKK)
C       CALL GSQBS2(NHKK)
C       CALL USQBS1(NHKK)
C       CALL USQBS2(NHKK)
C       CALL GSABS1(NHKK)
C       CALL GSABS2(NHKK)
C       CALL USABS1(NHKK)
C       CALL USABS2(NHKK)
C     ELSE
C       CALL GSQBS2(NHKK)
C       CALL GSQBS1(NHKK)
C       CALL USQBS2(NHKK)
C       CALL USQBS1(NHKK)
C       CALL GSABS2(NHKK)
C       CALL GSABS1(NHKK)
C       CALL USABS2(NHKK)
C       CALL USABS1(NHKK)
C     ENDIF
 
      IF ( DT_RNDM(vv).LE.0.5D0 ) THEN
         CALL DT_DBREAK(1)
         CALL DT_DBREAK(2)
         CALL DT_DBREAK(3)
         CALL DT_DBREAK(4)
         CALL DT_DBREAK(5)
         CALL DT_DBREAK(6)
         CALL DT_DBREAK(7)
         CALL DT_DBREAK(8)
      ELSE
         CALL DT_DBREAK(2)
         CALL DT_DBREAK(1)
         CALL DT_DBREAK(4)
         CALL DT_DBREAK(3)
         CALL DT_DBREAK(6)
         CALL DT_DBREAK(5)
         CALL DT_DBREAK(8)
         CALL DT_DBREAK(7)
      END IF
 
      END SUBROUTINE
