
      SUBROUTINE PHO_CPCINI(Nrows,Number,List)
C***********************************************************************
C
C     initialization of particle hash table
C
C     input:   Number     vector with Nrows entries according to PDG
C                         convention
C
C     output:  List       vector with hash table
C
C     (this code is based on the function initpns written by
C      Gerry Lynch, LBL, January 1990)
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      INTEGER Number(*) , List(*) , Nrows
 
      INTEGER nin , nout , ip , i
 
      DO i = 1 , 577
         List(i) = 0
      END DO
 
C    Loop over all of the elements in the Number vector
 
      DO ip = 1 , Nrows
         nin = Number(ip)
 
C    Calculate a list number for this particle id number
         IF ( nin.GT.99999 .OR. nin.LE.0 ) THEN
            nout = -1
         ELSE IF ( nin.LE.577 ) THEN
            nout = nin
         ELSE
            nout = MOD(nin,577)
 
         END IF
 
 50      IF ( nout.LT.0 ) THEN
C    Count the bad entries
            IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,i10)')
     &            'pho_cpcini: invalid particle ID' , nin
            GOTO 100
         END IF
         IF ( List(nout).EQ.0 ) THEN
            List(nout) = ip
         ELSE
            IF ( nin.EQ.Number(List(nout)) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,i10)')
     &               'pho_cpcini: double particle ID' , nin
            END IF
            nout = nout + 5
            IF ( nout.GT.577 ) nout = MOD(nout,577)
 
            GOTO 50
         END IF
 100  END DO
 
      END SUBROUTINE
