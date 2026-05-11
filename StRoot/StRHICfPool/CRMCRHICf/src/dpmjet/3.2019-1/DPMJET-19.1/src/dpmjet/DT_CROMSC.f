
      SUBROUTINE DT_CROMSC(Pin,R,Pout,Incl)
 
C***********************************************************************
C Cronin-Effect. Multiple scattering of one parton passing through     *
C nuclear matter.                                                      *
C            PIN(4)       input 4-momentum of parton                   *
C            POUT(4)      4-momentum of parton after mult. scatt.      *
C            R(3)         spatial position of parton in target nucleus *
C            INCL = 1     multiple sc. in projectile                   *
C                 = 2     multiple sc. in target                       *
C This is a revised version of the original version written by J. Ranft*
C This version dated 17.01.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION a , cfe , costh , cosx , cosxn , cosy , cosyn , 
     &                 cosz , coszn , dist , pe , Pin , Pout , ptot , 
     &                 pz , R , r1 , r2 , rncl , rtesq
      DOUBLE PRECISION sfe , sinth , theta , theto , TINY3 , tmp , ZERO
      INTEGER Incl , k , mode , ncback
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY3=1.0D-3)
 
      LOGICAL lstart
 
C rejection counter
      INCLUDE 'inc/dtrejc'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      DIMENSION Pin(4) , Pout(4) , R(3)
 
      DATA lstart/.TRUE./
 
      IRCron(1) = IRCron(1) + 1
 
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) CROnco
99010    FORMAT (/,1X,'CROMSC:  multiple scattering of chain ends',
     &           ' treated',/,10X,'with parameter CRONCO = ',F5.2)
         lstart = .FALSE.
      END IF
 
      ncback = 0
      rncl = RPRoj
      IF ( Incl.EQ.2 ) rncl = RTArg
 
C Lorentz-transformation into Lab.
      mode = -(Incl+1)
      CALL DT_LTNUC(Pin(3),Pin(4),pz,pe,mode)
 
      ptot = SQRT(Pin(1)**2+Pin(2)**2+pz**2)
      IF ( ptot.LE.8.0D0 ) THEN
 
         IRCron(2) = IRCron(2) + 1
      ELSE
 
C direction cosines of parton before mult. scattering
         cosx = Pin(1)/ptot
         cosy = Pin(2)/ptot
         cosz = pz/ptot
 
         rtesq = R(1)**2 + R(2)**2 + R(3)**2 - rncl**2
         IF ( rtesq.LT.-TINY3 ) THEN
 
C calculate distance (DIST) from R to surface of nucleus (radius RNCL)
C in the direction of particle motion
 
            a = cosx*R(1) + cosy*R(2) + cosz*R(3)
            tmp = a**2 - rtesq
            IF ( tmp.LT.ZERO ) THEN
               IRCron(3) = IRCron(3) + 1
            ELSE
               dist = -a + SQRT(tmp)
 
C multiple scattering angle
               theto = CROnco*SQRT(dist)/ptot
               IF ( theto.GT.0.1D0 ) theto = 0.1D0
 
C Gaussian sampling of spatial angle
 10            CALL DT_RANNOR(r1,r2)
               theta = ABS(r1*theto)
               IF ( theta.GT.0.3D0 ) THEN
                  IRCron(2) = IRCron(2) + 1
               ELSE
                  CALL DT_DSFECF(sfe,cfe)
                  costh = COS(theta)
                  sinth = SIN(theta)
 
C new direction cosines
                  CALL DT_MYTRAN(1,cosx,cosy,cosz,costh,sinth,sfe,cfe,
     &               cosxn,cosyn,coszn)
 
                  Pout(1) = cosxn*ptot
                  Pout(2) = cosyn*ptot
                  pz = coszn*ptot
C Lorentz-transformation into nucl.-nucl. cms
                  mode = Incl + 1
                  CALL DT_LTNUC(pz,pe,Pout(3),Pout(4),mode)
 
C     IF (ABS(PIN(4)-POUT(4)).GT.0.2D0) THEN
C     IF ( (ABS(PIN(4)-POUT(4))/PIN(4)).GT.0.1D0 ) THEN
                  IF ( (ABS(Pin(4)-Pout(4))/Pin(4)).GT.0.05D0 ) THEN
                     theto = theto/2.0D0
                     ncback = ncback + 1
                     IF ( MOD(ncback,200).EQ.0 ) THEN
 
                        IF ( LPRi.GT.4 ) WRITE (LOUt,99020) theto , 
     &                       Pin , Pout
99020                   FORMAT (1X,
     &                     'CROMSC: inconsistent scattering angle ',
     &                     E12.4,/,1X,'        PIN :',4E12.4,/,1X,
     &                     '       POUT:',4E12.4)
                        IRCron(2) = IRCron(2) + 1
                        GOTO 100
                     END IF
                     GOTO 10
                  END IF
 
                  RETURN
               END IF
            END IF
         END IF
      END IF
 
 100  DO k = 1 , 4
         Pout(k) = Pin(k)
      END DO
      END SUBROUTINE
