
      INTEGER FUNCTION IDT_IQUARK(K,Idbamj)
 
C***********************************************************************
C                                                                      *
C     quark contents according to PDG conventions                      *
C     (random selection in case of quark mixing)                       *
C                                                                      *
C     input:   IDBAMJ BAMJET particle code                             *
C              K      1..3   quark number                              *
C                                                                      *
C     output:  1   d  (anti --> neg.)                                  *
C              2   u                                                   *
C              3   s                                                   *
C              4   c                                                   *
C                                                                      *
C This version written by R. Engel.                                    *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Idbamj , IDT_IBJQUA , iq , K
      SAVE 
 
      iq = IDT_IBJQUA(K,Idbamj)
C quark-antiquark
      IF ( iq.GT.6 ) iq = 6 - iq
C exchange of up and down
      IF ( ABS(iq).EQ.1 ) THEN
         iq = SIGN(2,iq)
      ELSE IF ( ABS(iq).EQ.2 ) THEN
         iq = SIGN(1,iq)
      END IF
      IDT_IQUARK = iq
 
      END FUNCTION
