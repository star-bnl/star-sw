
      DOUBLE PRECISION FUNCTION DT_BETREJ(Gam,Eta,Xmin,Xmax)
 
      IMPLICIT NONE
      DOUBLE PRECISION betmax , betxx , DT_RNDM , Eta , Gam , ONE , 
     &                 Xmax , Xmin , xx , yy
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ONE=1.0D0)
 
      IF ( Xmin.GE.Xmax ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Xmin , Xmax
99010    FORMAT (1X,'DT_BETREJ:  XMIN<XMAX execution stopped ',2F10.5)
         STOP
      END IF
 
 100  xx = Xmin + (Xmax-Xmin)*DT_RNDM(Eta)
      betmax = Xmin**(Gam-ONE)*(ONE-Xmin)**(Eta-ONE)
      yy = betmax*DT_RNDM(xx)
      betxx = xx**(Gam-ONE)*(ONE-xx)**(Eta-ONE)
      IF ( yy.GT.betxx ) GOTO 100
      DT_BETREJ = xx
 
      END FUNCTION
