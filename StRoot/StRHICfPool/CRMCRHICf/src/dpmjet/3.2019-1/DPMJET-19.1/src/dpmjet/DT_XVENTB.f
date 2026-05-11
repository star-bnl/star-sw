
      SUBROUTINE DT_XVENTB(Ncsy,Irej)
C     SUBROUTINE DT_EVENTB(NCSY,IREJ)
 
      IMPLICIT NONE
      INTEGER Irej , Ncsy
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010 FORMAT (1X,'EVENTB:   PHOJET-package requested but not linked!')
      STOP
 
      END SUBROUTINE
