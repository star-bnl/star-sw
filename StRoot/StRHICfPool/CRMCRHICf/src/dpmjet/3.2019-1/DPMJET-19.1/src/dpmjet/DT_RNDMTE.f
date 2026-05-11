
      SUBROUTINE DT_RNDMTE(Io)
 
      IMPLICIT NONE
      DOUBLE PRECISION cc , ccd , ccm , d , DT_RNDM , sd , u , uu , x , 
     &                 xx
      INTEGER ii , ii1 , ii2 , Io , jj
      SAVE 
 
      DIMENSION uu(97) , u(6) , x(6) , d(6)
      DATA u/6533892.D0 , 14220222.D0 , 7275067.D0 , 6172232.D0 , 
     &     8354498.D0 , 10633180.D0/
 
      CALL DT_RNDMOU(uu,cc,ccd,ccm,ii,jj)
      CALL DT_RNDMST(12,34,56,78)
      DO ii1 = 1 , 20000
         xx = DT_RNDM(xx)
      END DO
      sd = 0.0D0
      DO ii2 = 1 , 6
         x(ii2) = 4096.D0*(4096.D0*DT_RNDM(sd))
         d(ii2) = x(ii2) - u(ii2)
         sd = sd + d(ii2)
      END DO
      CALL DT_RNDMIN(uu,cc,ccd,ccm,ii,jj)
C*sr 24.01.95
C     IF ( IO.EQ. 1.OR. SD.NE.0. 0) WRITE(6,500) (U(I),X(I),D(I),I=1,6)
      IF ( (Io.EQ.1) .OR. (sd.NE.0.0) ) THEN
C        WRITE(6,1000)
C1000    FORMAT(/,/,1X,'DT_RNDMTE: Test of random-number generator...',
C    &          ' passed')
      END IF
C*
C 500 FORMAT('  === TEST OF THE RANDOM-GENERATOR ===',/,
C    &'    EXPECTED VALUE    CALCULATED VALUE     DIFFERENCE',/, 6(F17.
C    &1,F20.1,F15.3,/), '  === END OF TEST ;',
C    &'  GENERATOR HAS THE SAME STATUS AS BEFORE CALLING DT_RNDMTE')
      END SUBROUTINE
