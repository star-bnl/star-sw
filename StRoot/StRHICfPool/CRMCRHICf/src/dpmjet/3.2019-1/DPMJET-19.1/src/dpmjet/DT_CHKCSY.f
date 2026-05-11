
      SUBROUTINE DT_CHKCSY(Id1,Id2,Lchk)
 
C***********************************************************************
C CHeCk Chain SYstem for consistency of partons at chain ends.         *
C            ID1,ID2        PDG-numbers of partons at chain ends       *
C            LCHK = .true.  consistent chain                           *
C                 = .false. inconsistent chain                         *
C This version dated 18.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Id1 , Id2
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL Lchk
 
      Lchk = .TRUE.
 
C q-aq chain
      IF ( (ABS(Id1).LE.6) .AND. (ABS(Id2).LE.6) ) THEN
         IF ( Id1*Id2.GT.0 ) Lchk = .FALSE.
C q-qq, aq-aqaq chain
      ELSE IF ( ((ABS(Id1).LE.6) .AND. (ABS(Id2).GT.6)) .OR. 
     &          ((ABS(Id1).GT.6) .AND. (ABS(Id2).LE.6)) ) THEN
         IF ( Id1*Id2.LT.0 ) Lchk = .FALSE.
C qq-aqaq chain
      ELSE IF ( (ABS(Id1).GT.6) .AND. (ABS(Id2).GT.6) ) THEN
         IF ( Id1*Id2.GT.0 ) Lchk = .FALSE.
      END IF
 
      END SUBROUTINE
