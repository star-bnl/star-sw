
      SUBROUTINE PHO_HARCOR(Icold,Icnew)
C***********************************************************************
C
C     substituite color in /POEVT2/
C
C     input:    ICOLD   old color
C               ICNEW   new color
C
C***********************************************************************
      IMPLICIT NONE
      INTEGER i , Icnew , Icold
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DO i = NHEp , 3 , -1
         IF ( ISThep(i).EQ.-1 ) THEN
            IF ( ICOlor(1,i).EQ.Icold ) THEN
               ICOlor(1,i) = Icnew
               RETURN
            ELSE IF ( IDHep(i).EQ.21 ) THEN
               IF ( ICOlor(2,i).EQ.Icold ) THEN
                  ICOlor(2,i) = Icnew
                  RETURN
               END IF
            END IF
C       ELSE IF(ISTHEP(I).EQ.20) THEN
C         IF(ICOLOR(1,I).EQ.-ICOLD) THEN
C           print LO,' PHO_HARCOR(3): line, old, new:',I,ICOLD,ICNEW
C           ICOLOR(1,I) = -ICNEW
C           RETURN
C         ELSE IF(IDHEP(I).EQ.21) THEN
C           IF(ICOLOR(2,I).EQ.-ICOLD) THEN
C             print LO,' PHO_HARCOR(4): line, old, new:',I,ICOLD,ICNEW
C             ICOLOR(2,I) = -ICNEW
C             RETURN
C           ENDIF
C         ENDIF
         END IF
      END DO
      END SUBROUTINE
