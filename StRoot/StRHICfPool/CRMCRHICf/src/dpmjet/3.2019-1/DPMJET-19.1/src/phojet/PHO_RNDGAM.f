
      DOUBLE PRECISION FUNCTION PHO_RNDGAM(Alam,Eta)
C********************************************************************
C
C     RANDOM NUMBER SELECTION FROM GAMMA DISTRIBUTION
C     F(X) = ALAM**ETA*X**(ETA-1)*EXP(-ALAM*X) / GAM(ETA)
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Alam , DT_RNDM , Eta , f , r , y , yyy , z
      INTEGER i , n , ncou
      SAVE 
C
      ncou = 0
      n = Eta
      f = Eta - n
      IF ( f.EQ.0.D0 ) THEN
         y = 0.D0
      ELSE
 50      r = DT_RNDM(Eta)
         ncou = ncou + 1
         IF ( ncou.GE.11 ) THEN
            y = 0.D0
         ELSE
            IF ( r.LT.f/(f+2.71828D0) ) THEN
               y = 1.D0 - LOG(DT_RNDM(r)+1.0D-9)
               IF ( DT_RNDM(y).GT.y**(f-1.D0) ) GOTO 50
            ELSE
               yyy = LOG(DT_RNDM(f)+1.0D-9)/f
               IF ( ABS(yyy).GT.50.D0 ) THEN
                  y = 0.D0
                  GOTO 100
               ELSE
                  y = EXP(yyy)
                  IF ( LOG(DT_RNDM(y)+1.0D-9).GT.-y ) GOTO 50
               END IF
            END IF
            IF ( n.EQ.0 ) GOTO 200
         END IF
      END IF
 100  z = 1.D0
      DO i = 1 , n
         z = z*DT_RNDM(y)
      END DO
      y = y - LOG(z+1.0D-9)
 200  PHO_RNDGAM = y/Alam
      END FUNCTION
