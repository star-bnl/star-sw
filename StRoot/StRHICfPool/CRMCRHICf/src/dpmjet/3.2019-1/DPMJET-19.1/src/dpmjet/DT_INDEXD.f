
      SUBROUTINE DT_INDEXD(Ka,Kb,Ind)
 
      IMPLICIT NONE
      INTEGER Ind , Ka , Kb , kp , ks
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      kp = Ka*Kb
      ks = Ka + Kb
      IF ( kp.EQ.1 ) Ind = 1
      IF ( kp.EQ.2 ) Ind = 2
      IF ( kp.EQ.3 ) Ind = 3
      IF ( (kp.EQ.4) .AND. (ks.EQ.5) ) Ind = 4
      IF ( kp.EQ.5 ) Ind = 5
      IF ( (kp.EQ.6) .AND. (ks.EQ.7) ) Ind = 6
      IF ( (kp.EQ.4) .AND. (ks.EQ.4) ) Ind = 7
      IF ( (kp.EQ.6) .AND. (ks.EQ.5) ) Ind = 8
      IF ( kp.EQ.8 ) Ind = 9
      IF ( kp.EQ.10 ) Ind = 10
      IF ( (kp.EQ.12) .AND. (ks.EQ.8) ) Ind = 11
      IF ( kp.EQ.9 ) Ind = 12
      IF ( (kp.EQ.12) .AND. (ks.EQ.7) ) Ind = 13
      IF ( kp.EQ.15 ) Ind = 14
      IF ( kp.EQ.18 ) Ind = 15
      IF ( kp.EQ.16 ) Ind = 16
      IF ( kp.EQ.20 ) Ind = 17
      IF ( kp.EQ.24 ) Ind = 18
      IF ( kp.EQ.25 ) Ind = 19
      IF ( kp.EQ.30 ) Ind = 20
      IF ( kp.EQ.36 ) Ind = 21
 
      END SUBROUTINE
