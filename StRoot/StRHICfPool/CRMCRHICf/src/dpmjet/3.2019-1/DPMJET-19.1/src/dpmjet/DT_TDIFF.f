
      DOUBLE PRECISION FUNCTION DT_TDIFF(Ecm,Tmin,Xm1i,K1,Xm2i,K2,Irej)
 
C***********************************************************************
C t-selection for single/double diffractive interactions.              *
C          ECM      cm. energy                                         *
C          TMIN     minimum momentum transfer to produce diff. masses  *
C          XM1/XM2  diffractively produced masses                      *
C                   (for single diffraction XM2 is obsolete)           *
C          K1/K2= 0 not excited                                        *
C               = 1 low-mass excitation                                *
C               = 2 high-mass excitation                               *
C This version dated 11.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ALPHAP , BTP0 , DT_RNDM , Ecm , slope , t , 
     &                 Tmin , xdi , xm1 , Xm1i , xm2 , Xm2i , y , ZERO
      INTEGER Irej , K1 , K2 , ncloop
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0)
 
      PARAMETER (BTP0=3.7D0,ALPHAP=0.24D0)
 
      Irej = 0
      ncloop = 0
      DT_TDIFF = ZERO
 
      IF ( K1.GT.0 ) THEN
         xm1 = Xm1i
         xm2 = Xm2i
      ELSE
         xm1 = Xm2i
      END IF
      xdi = (xm1/Ecm)**2
      IF ( (K1.EQ.0) .OR. (K2.EQ.0) ) THEN
C slope for single diffraction
         slope = BTP0 - 2.0D0*ALPHAP*LOG(xdi)
      ELSE
C slope for double diffraction
         slope = -2.0D0*ALPHAP*LOG(xdi*xm2**2)
      END IF
 
 100  ncloop = ncloop + 1
      IF ( MOD(ncloop,1000).EQ.0 ) THEN
 
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Ecm , Tmin , Xm1i , Xm2i , 
     &        K1 , K2
99010    FORMAT (1X,'DT_TDIFF:   t-selection rejected!',/,1X,'ECM  = ',
     &           E12.3,' TMIN = ',E12.2,/,1X,'XM1I = ',E12.3,' XM2I = ',
     &           E12.3,' K1 = ',I2,' K2 = ',I2)
         Irej = 1
      ELSE
         y = DT_RNDM(xdi)
         t = -LOG(1.0D0-y)/slope
         IF ( ABS(t).LE.ABS(Tmin) ) GOTO 100
         DT_TDIFF = -ABS(t)
      END IF
 
      END FUNCTION
