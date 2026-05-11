
      DOUBLE PRECISION FUNCTION DT_DGAMRN(Alam,Eta)
 
C***********************************************************************
C Sampling from Gamma-distribution.                                    *
C       F(X) = ALAM**ETA*X**(ETA-1)*EXP(-ALAM*X) / GAM(ETA)            *
C Processed by S. Roesler, 6.5.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Alam , DT_RNDM , Eta , f , ONE , r , TINY9 , y , 
     &                 yyy , z , ZERO
      INTEGER i , n , ncou
      SAVE 
      PARAMETER (ZERO=0.0D0,TINY9=1.0D-9,ONE=1.0D0)
 
      ncou = 0
      n = INT(Eta)
      f = Eta - DBLE(n)
      IF ( f.EQ.ZERO ) THEN
         y = 0.0D0
      ELSE
 50      r = DT_RNDM(f)
         ncou = ncou + 1
         IF ( ncou.GE.11 ) THEN
            y = 0.0D0
         ELSE
            IF ( r.LT.f/(f+2.71828D0) ) THEN
               y = ONE - LOG(DT_RNDM(y)+TINY9)
               IF ( DT_RNDM(r).GT.y**(f-ONE) ) GOTO 50
            ELSE
               yyy = LOG(DT_RNDM(r)+TINY9)/f
               IF ( ABS(yyy).GT.50.0D0 ) THEN
                  y = 0.0D0
                  GOTO 100
               ELSE
                  y = EXP(yyy)
                  IF ( LOG(DT_RNDM(y)+TINY9).GT.-y ) GOTO 50
               END IF
            END IF
            IF ( n.EQ.0 ) GOTO 200
         END IF
      END IF
 100  z = 1.0D0
      DO i = 1 , n
         z = z*DT_RNDM(z)
      END DO
      y = y - LOG(z+TINY9)
 200  DT_DGAMRN = y/Alam
 
      END FUNCTION
