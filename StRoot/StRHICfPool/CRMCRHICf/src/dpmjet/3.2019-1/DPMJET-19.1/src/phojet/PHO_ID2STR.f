
      SUBROUTINE PHO_ID2STR(Id1,Id2,Nobam,Ibam1,Ibam2,Ibam3,Ibam4)
C*********************************************************************
C
C     conversion of quark numbering scheme
C
C     input:   standard particle codes:
C                       ID1
C                       ID2
C
C     output:  NOBAM    CPC string code
C              quark codes (PDG convention):
C                       IBAM1
C                       IBAM2
C                       IBAM3
C                       IBAM4
C
C              NOBAM = -1 invalid flavour combinations
C
C*********************************************************************
      IMPLICIT NONE
      INTEGER Ibam1 , Ibam2 , Ibam3 , Ibam4 , Id1 , Id2 , ida1 , ida2 , 
     &        Nobam
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      ida1 = ABS(Id1)
      ida2 = ABS(Id2)
 
C  quark-antiquark string
      IF ( (ida1.LE.6) .AND. (ida2.LE.6) ) THEN
         IF ( (Id1*Id2).GE.0 ) GOTO 100
         Ibam1 = Id1
         Ibam2 = Id2
         Ibam3 = 0
         Ibam4 = 0
         Nobam = 3
C  quark-diquark string
      ELSE IF ( (ida2.GT.6) .AND. (ida1.LE.6) ) THEN
         IF ( (Id1*Id2).LE.0 ) GOTO 100
         Ibam1 = Id1
         Ibam2 = Id2/1000
         Ibam3 = (Id2-Ibam2*1000)/100
         Ibam4 = 0
         Nobam = 4
C  diquark-quark string
      ELSE IF ( (ida1.GT.6) .AND. (ida2.LE.6) ) THEN
         IF ( (Id1*Id2).LE.0 ) GOTO 100
         Ibam1 = Id1/1000
         Ibam2 = (Id1-Ibam1*1000)/100
         Ibam3 = Id2
         Ibam4 = 0
         Nobam = 6
C  gluon-gluon string
      ELSE IF ( (ida1.EQ.21) .AND. (ida2.EQ.21) ) THEN
         Ibam1 = 21
         Ibam2 = 21
         Ibam3 = 0
         Ibam4 = 0
         Nobam = 7
C  diquark-antidiquark string
      ELSE IF ( (ida1.GT.6) .AND. (ida2.GT.6) ) THEN
         IF ( (Id1*Id2).GE.0 ) GOTO 100
         Ibam1 = Id1/1000
         Ibam2 = (Id1-Ibam1*1000)/100
         Ibam3 = Id2/1000
         Ibam4 = (Id2-Ibam3*1000)/100
         Nobam = 5
      END IF
      RETURN
 
C  invalid combination
 100  IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,2I10)') 
     &                'PHO_ID2STR: invalid flavors for string (ID1,ID2)'
     &                , Id1 , Id2
      CALL PHO_ABORT
 
      END SUBROUTINE
