
      INTEGER FUNCTION IDT_IPDG2B(Id,Nn,Mode)
 
C***********************************************************************
C                                                                      *
C     conversion of quark numbering scheme                             *
C                                                                      *
C     input:   PDG parton numbering                                    *
C              for diquarks:  NN number of the constituent quark       *
C                             (e.g. ID=2301,NN=1 -> ICONV2=1)          *
C                                                                      *
C     output:  BAMJET particle codes                                   *
C              1 u     7 a-u   (MODE=1)  -1 a-u   (MODE=2)             *
C              2 d     8 a-d             -2 a-d                        *
C              3 s     9 a-s             -3 a-s                        *
C              4 c    10 a-c             -4 a-c                        *
C                                                                      *
C This is a modified version of ICONV2 written by R. Engel.            *
C This version dated 13.12.94 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Id , ida , kf , Mode , Nn
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      ida = ABS(Id)
C diquarks
      IF ( ida.GT.6 ) THEN
         kf = 3
         IF ( ida.GE.1000 ) kf = 4
         ida = ida/(10**(kf-Nn))
         ida = MOD(ida,10)
      END IF
C exchange up and dn quarks
      IF ( ida.EQ.1 ) THEN
         ida = 2
      ELSE IF ( ida.EQ.2 ) THEN
         ida = 1
      END IF
C antiquarks
      IF ( Id.LT.0 ) THEN
         IF ( Mode.EQ.1 ) THEN
            ida = ida + 6
         ELSE
            ida = -ida
         END IF
      END IF
      IDT_IPDG2B = ida
 
      END FUNCTION
