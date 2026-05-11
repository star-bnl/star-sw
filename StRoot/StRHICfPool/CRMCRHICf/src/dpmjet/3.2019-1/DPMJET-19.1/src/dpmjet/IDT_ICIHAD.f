
      INTEGER FUNCTION IDT_ICIHAD(Mcind)
 
C***********************************************************************
C Conversion of particle index PDG proposal --> BAMJET-index scheme    *
C This is a completely new version dated 25.10.95.                     *
C Renamed to be not in conflict with the modified PHOJET-version       *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , jsign , kpdg , Mcind
      SAVE 
 
C hadron index conversion (BAMJET <--> PDG)
      INCLUDE 'inc/dthaic'
 
      IDT_ICIHAD = 0
      kpdg = ABS(Mcind)
      IF ( (kpdg.EQ.0) .OR. (kpdg.GT.70000) ) RETURN
      IF ( Mcind.LT.0 ) THEN
         jsign = 1
      ELSE
         jsign = 2
      END IF
      IF ( kpdg.GE.10000 ) THEN
         DO i = 1 , 19
            IDT_ICIHAD = IBAm5(jsign,i)
            IF ( IPDg5(jsign,i).EQ.Mcind ) GOTO 99999
            IDT_ICIHAD = 0
         END DO
      ELSE IF ( kpdg.GE.1000 ) THEN
         DO i = 1 , 29
            IDT_ICIHAD = IBAm4(jsign,i)
            IF ( IPDg4(jsign,i).EQ.Mcind ) GOTO 99999
            IDT_ICIHAD = 0
         END DO
      ELSE IF ( kpdg.GE.100 ) THEN
         DO i = 1 , 22
            IDT_ICIHAD = IBAm3(jsign,i)
            IF ( IPDg3(jsign,i).EQ.Mcind ) GOTO 99999
            IDT_ICIHAD = 0
         END DO
      ELSE IF ( kpdg.GE.10 ) THEN
         DO i = 1 , 7
            IDT_ICIHAD = IBAm2(jsign,i)
            IF ( IPDg2(jsign,i).EQ.Mcind ) GOTO 99999
            IDT_ICIHAD = 0
         END DO
      END IF
 
99999 END FUNCTION
