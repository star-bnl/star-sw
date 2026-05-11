
      SUBROUTINE PHO_TRACE(Istart,Iswi,Level)
C**********************************************************************
C
C     trace program subroutines according to level,
C                          original output levels will be saved
C
C     input:   ISTART      first event to trace
C              ISWI        number of events to trace
C                                0   loop call, use old values
C                               -1   restore original output levels
C                                1   store level and wait for event
C              LEVEL       desired output level
C                                0   standard output
C                                3   internal rejections
C                                5   cross sections, slopes etc.
C                               10   parameter of subroutines and
C                                    results
C                               20   huge amount of debug output
C                               30   maximal possible output
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , ilevel , imem , ioff , ion , Istart , isw , Iswi , 
     &        Level
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
      DIMENSION imem(NMAXD)
 
C  protect ISWI
      isw = Iswi
 100  IF ( isw.EQ.0 ) THEN
         IF ( KEVent.LT.ion ) THEN
            RETURN
         ELSE IF ( KEVent.EQ.ion ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(///,1X,A,///)')
     &            'PHO_TRACE: trace mode switched on'
            DO i = 1 , NMAXD
               imem(i) = IDEb(i)
               IDEb(i) = MAX(ilevel,imem(i))
            END DO
         ELSE IF ( KEVent.EQ.ioff ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(//,1X,A,///)')
     &            'PHO_TRACE: trace mode switched off'
            DO i = 1 , NMAXD
               IDEb(i) = imem(i)
            END DO
         END IF
      ELSE IF ( isw.EQ.-1 ) THEN
         DO i = 1 , NMAXD
            IDEb(i) = imem(i)
         END DO
      ELSE
C  save information
         ion = Istart
         ioff = Istart + isw
         ilevel = Level
      END IF
C  check coincidence
      IF ( isw.GT.0 ) THEN
         isw = 0
         ilevel = Level
         GOTO 100
      END IF
 
      END SUBROUTINE
