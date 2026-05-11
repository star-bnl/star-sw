
      SUBROUTINE DT_JSPARA(Mode)
 
      IMPLICIT NONE
      DOUBLE PRECISION diff , ONE , qarj , qaru , TINY1 , TINY10 , 
     &                 TINY3 , ZERO
      INTEGER i , istj , istu , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3,TINY1=1.0D-1,ONE=1.0D0,
     &           ZERO=0.0D0)
 
      LOGICAL lfirst
 
      INCLUDE 'inc/pydat1'
 
      DIMENSION istu(200) , qaru(200) , istj(200) , qarj(200)
 
      DATA lfirst/.TRUE./
 
C save the default JETSET-parameter on the first call
      IF ( lfirst ) THEN
         DO i = 1 , 200
            istu(i) = MSTu(i)
            qaru(i) = PARu(i)
            istj(i) = MSTj(i)
            qarj(i) = PARj(i)
         END DO
         lfirst = .FALSE.
      END IF
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010 FORMAT (1X,'DT_JSPARA: new value (default value)')
 
C compare the default JETSET-parameter with the present values
      DO i = 1 , 200
         IF ( (MSTu(i).NE.istu(i)) .AND. (i.NE.31) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) 'MSTU(' , i , MSTu(i) , 
     &           istu(i)
C           ISTU(I) = MSTU(I)
         END IF
         diff = ABS(PARu(i)-qaru(i))
         IF ( (diff.GE.1.0D-5) .AND. (i.NE.21) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) 'PARU(' , i , PARu(i) , 
     &           qaru(i)
C           QARU(I) = PARU(I)
         END IF
         IF ( MSTj(i).NE.istj(i) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) 'MSTJ(' , i , MSTj(i) , 
     &           istj(i)
C           ISTJ(I) = MSTJ(I)
         END IF
         diff = ABS(PARj(i)-qarj(i))
         IF ( diff.GE.1.0D-5 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) 'PARJ(' , i , PARj(i) , 
     &           qarj(i)
C           QARJ(I) = PARJ(I)
         END IF
      END DO
99020 FORMAT (12X,A5,I3,'): ',F6.3,' (',F6.3,')')
99030 FORMAT (12X,A5,I3,'): ',I6,' (',I6,')')
 
      END SUBROUTINE
