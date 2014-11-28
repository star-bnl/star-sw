
C **********************************************************************

      SUBROUTINE DVNOPT

      IMPLICIT NONE

C...Change of default options in DIVONNE

      COMMON /PRINT/ IPRDIV
      INTEGER IPRDIV

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/


      IPRDIV=0
      IF(LST3.GE.2) IPRDIV=1000
      IF(LST3.GE.4) IPRDIV=10
      IF(LST3.GE.4) WRITE(6,1000) IPRDIV
      RETURN
 1000 FORMAT(5X,'DIVON4 print flag changed: IPRDIV =',I5)
      END
