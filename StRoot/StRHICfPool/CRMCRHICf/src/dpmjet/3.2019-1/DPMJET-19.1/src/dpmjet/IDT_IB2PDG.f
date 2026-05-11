
      INTEGER FUNCTION IDT_IB2PDG(Id1,Id2,Mode)
 
C***********************************************************************
C                                                                      *
C     conversion of quark numbering scheme                             *
C                                                                      *
C     input:   BAMJET particle codes                                   *
C              1 u     7 a-u   (MODE=1)  -1 a-u   (MODE=2)             *
C              2 d     8 a-d             -2 a-d                        *
C              3 s     9 a-s             -3 a-s                        *
C              4 c    10 a-c             -4 a-c                        *
C                                                                      *
C     output:  PDG parton numbering                                    *
C                                                                      *
C This version dated 13.12.94 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Id1 , Id2 , ida , idb , ihkkq , ihkkqq , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      DIMENSION ihkkq(-6:6) , ihkkqq(-3:3,-3:3)
      DATA ihkkq/ - 6 , -5 , -4 , -3 , -1 , -2 , 0 , 2 , 1 , 3 , 4 , 5 , 
     &     6/
      DATA ihkkqq/ - 3303 , -3103 , -3203 , 0 , 0 , 0 , 0 , -3103 , 
     &     -1103 , -2103 , 0 , 0 , 0 , 0 , -3203 , -2103 , -2203 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     2203 , 2103 , 3203 , 0 , 0 , 0 , 0 , 2103 , 1103 , 3103 , 0 , 
     &     0 , 0 , 0 , 3203 , 3103 , 3303/
 
      ida = Id1
      idb = Id2
      IF ( Mode.EQ.1 ) THEN
         IF ( Id1.GT.6 ) ida = -(Id1-6)
         IF ( Id2.GT.6 ) idb = -(Id2-6)
      END IF
      IF ( Id2.EQ.0 ) THEN
         IDT_IB2PDG = ihkkq(ida)
      ELSE
C**anfe took diquark assembling from phojet due to charmed baryons
C**warning side effect: PARMDL(135) fraction of spin-1 diq. active
         IDT_IB2PDG = ihkkqq(ida,idb)
         ! IDT_IB2PDG = ipho_diqu(IHKKQ(IDA),IHKKQ(IDB))
      END IF
 
 
      END FUNCTION
