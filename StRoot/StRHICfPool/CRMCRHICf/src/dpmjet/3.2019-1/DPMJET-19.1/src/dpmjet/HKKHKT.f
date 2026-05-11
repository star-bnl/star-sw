
      SUBROUTINE HKKHKT(I,J)
      IMPLICIT NONE
      INTEGER I , J
      SAVE 
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      INCLUDE 'inc/hkktmp'
C
      ISThkk(I) = ISThkt(J)
      IDHkk(I) = IDHkt(J)
C     IF(J.EQ.3.OR.J.EQ.6.OR.J.EQ.9)THEN
      IF ( IDHkk(I).EQ.88888 ) THEN
C       JMOHKK(1,I)=I-2
C       JMOHKK(2,I)=I-1
         JMOhkk(1,I) = I - (J-JMOhkt(1,J))
         JMOhkk(2,I) = I - (J-JMOhkt(2,J))
      ELSE
         JMOhkk(1,I) = JMOhkt(1,J)
         JMOhkk(2,I) = JMOhkt(2,J)
      END IF
      JDAhkk(1,I) = JDAhkt(1,J)
      JDAhkk(2,I) = JDAhkt(2,J)
C       IF(J.EQ.1.OR.J.EQ.4.OR.J.EQ.7)THEN
C       JDAHKK(1,I)=I+2
C     ELSEIF(J.EQ.2.OR.J.EQ.5.OR.J.EQ.8)THEN
C       JDAHKK(1,I)=I+1
C     ENDIF
      IF ( JDAhkt(1,J).GT.0 ) JDAhkk(1,I) = I + (JDAhkt(1,J)-J)
      PHKk(1,I) = PHKt(1,J)
      PHKk(2,I) = PHKt(2,J)
      PHKk(3,I) = PHKt(3,J)
      PHKk(4,I) = PHKt(4,J)
      PHKk(5,I) = PHKt(5,J)
      VHKk(1,I) = VHKt(1,J)
      VHKk(2,I) = VHKt(2,J)
      VHKk(3,I) = VHKt(3,J)
      VHKk(4,I) = VHKt(4,J)
      WHKk(1,I) = WHKt(1,J)
      WHKk(2,I) = WHKt(2,J)
      WHKk(3,I) = WHKt(3,J)
      WHKk(4,I) = WHKt(4,J)
      END SUBROUTINE
