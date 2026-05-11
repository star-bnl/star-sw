
      SUBROUTINE PHO_SELPT(Ee,Ptlow,Pthigh,Pts,Beta,Imode)
C***********************************************************************
C
C    select pt from different distributions
C
C    input:    EE            energy (for initialization only)
C                            otherwise x value of corresponding parton
C              PTLOW         lower pt limit
C              PTHIGH        upper pt limit
C                            (PTHIGH > 20 will cause DEXP underflows)
C
C              IMODE = 0     dNs/dP_t = P_t * ASS * exp(-BETA*P_t**2)
C              IMODE = 1     dNs/dP_t = P_t * ASS * exp(-BETA*P_t)
C              IMODE = 2     dNs/dP_t according photon wave function
C              IMODE = 10    no sampling
C
C              IMODE = -100+IMODE    initialization according to
C                                    given limitations
C
C    output:   PTS           sampled pt value
C    initialization:
C              BETA          soft pt slope in central region
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION aa , AMIN , Beta , betlo , betup , DEPS , 
     &                 DT_RNDM , Ee , EPS , fac1 , fac2 , p2 , 
     &                 PHO_DZEROX , PI2 , Pthigh , Ptlow , Pts , 
     &                 ptsmax , ptsmin , px
      DOUBLE PRECISION xi1 , xidel , ximin , xmt , xmt2 , xtol , xv
      INTEGER Imode , init , maxf , method
      SAVE 
 
      PARAMETER (PI2=6.28318530718D0,AMIN=1.D-2,EPS=1.D-7,DEPS=1.D-30)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  average number of cut soft and hard ladders (obsolete)
      INCLUDE 'inc/point2'
C  data needed for soft-pt calculation
      INCLUDE 'inc/point3'
 
      DOUBLE PRECISION PHO_CONN0 , PHO_CONN1
      EXTERNAL PHO_CONN0 , PHO_CONN1
 
C  initialization
 
      IF ( Imode.LT.0 ) THEN
 
C  initialization
         ptsmin = Ptlow
         ptsmax = Pthigh
         PTCon = Pthigh
C  calculation of parameters
         init = Imode + 100
         AAS = 0.D0
 
C  initialization for model 0 (gaussian pt distribution)
 
         IF ( init.EQ.0 ) THEN
            BETas(1) = PARmdl(23) + 0.15D0*LOG(Ee)
     &                 *(PARmdl(24)-PARmdl(23))
            betup = BETas(1)
            betlo = -2.D0
            xtol = PHO_CONN0(betlo)*PHO_CONN0(betup)
            IF ( xtol.LT.0.D0 ) THEN
               xtol = 1.D-4
               method = 1
               maxf = 500
               Beta = 0.D0
               Beta = PHO_DZEROX(betlo,betup,xtol,maxf,PHO_CONN0,method)
C           IF(BETA.LT.-1.D+10) THEN
C             WRITE(LO,'(1X,2A,1P,2E11.3)') 'PHO_SELPT: no Beta found ',
C    &          '(model 0: Ecm,PTcut)',EE,PTCON
C             WRITE(LO,'(1X,A,1P,3E10.3)')
C    &          'PHO_SELPT: SIGS,SIGH,DSIGHP',SIGS,SIGH,DSIGHP
C             CALL PHO_PREVNT(-1)
C             BETA = 0.01
C           ELSE
               AAS = DSIghp/PTCon*EXP(-Beta*PTCon**2)
C           ENDIF
            ELSE
               AAS = 0.D0
               Beta = BETas(1)
            END IF
 
C  initialization for model 1 (exponential pt distribution)
 
         ELSE IF ( init.EQ.1 ) THEN
            xmt = PARmdl(43)
            xmt2 = xmt*xmt
            BETas(1) = PARmdl(21) + 0.15D0*LOG(Ee)
     &                 *(PARmdl(22)-PARmdl(21))
            betup = BETas(1)
            betlo = -3.D0
            xtol = PHO_CONN1(betlo)*PHO_CONN1(betup)
            IF ( xtol.LT.0.D0 ) THEN
               xtol = 1.D-4
               method = 1
               maxf = 500
               Beta = 0.D0
               Beta = PHO_DZEROX(betlo,betup,xtol,maxf,PHO_CONN1,method)
C           IF(BETA.LT.-1.D+10) THEN
C             WRITE(LO,'(1X,2A,1P,2E11.3)') 'PHO_SELPT: no Beta found ',
C    &          '(model 1: Ecm,PTcut)',EE,PTCON
C             WRITE(LO,'(1X,A,1P,3E10.3)')
C    &          'PHO_SELPT: SIGS,SIGH,DSIGHP',SIGS,SIGH,DSIGHP
C             CALL PHO_PREVNT(-1)
C             BETA = 0.01
C           ELSE
               AAS = DSIghp/PTCon*EXP(-Beta*PTCon)
C           ENDIF
            ELSE
               AAS = 0.D0
               Beta = BETas(1)
            END IF
         ELSE IF ( init.EQ.10 ) THEN
            IF ( IDEb(5).GT.10 .AND. LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &            'PHO_SELPT: no soft pt sampling'
            RETURN
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &            'PHO_SELPT:ERROR: invalid distribution' , init
            CALL PHO_ABORT
         END IF
         Beta = MIN(Beta,BETas(1))
 
C  hard cross section is too big: neg. beta parameter
         IF ( Beta.LE.0.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,2E12.3)')
     &            'PHO_SELPT: parameter BETA negative (BETA,AAS)' , 
     &           Beta , AAS
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E11.3)')
     &            'SIGS,DSIGHP,SIGH,PTCON:' , SIGs , DSIghp , SIGh , 
     &           PTCon
            CALL PHO_PREVNT(-1)
         END IF
 
C  output of initialization parameters
         IF ( IDEb(5).GE.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &            'PHO_SELPT: initialization for model' , init
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,2E13.3)')
     &            'BETA,AAS        ' , Beta , AAS
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,3E13.3)')
     &            'ECM,PTMIN,PTMAX ' , Ee , ptsmin , ptsmax
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,3E13.3)')
     &            'SIGS,DSIGHP,SIGH' , SIGs , DSIghp , SIGh
         END IF
         GOTO 99999
      ELSE
 
         px = Pthigh
         Pts = 0.D0
 
C  initial checks
 
         IF ( px.LT.AMIN ) RETURN
 
         IF ( (px-Ptlow).LT.0.01 ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(5).GE.3 )
     &            WRITE (LO,'(1X,A,2E12.3,I3)')
     &            'PHO_SELPT: PTLOW,PTHIGH,IMODE ' , Ptlow , Pthigh , 
     &           Imode
            RETURN
         END IF
 
C  sampling of pt values according to IMODE
 
         IF ( Imode.EQ.0 ) THEN
 
            fac1 = EXP(-Beta*px**2)
            fac2 = (1.D0-fac1)
 20         xi1 = DT_RNDM(px)*fac2 + fac1
            Pts = SQRT(-1.D0/Beta*LOG(xi1))
            IF ( (Pts.GT.Pthigh) .OR. (Pts.LT.Ptlow) ) GOTO 20
 
         ELSE IF ( Imode.EQ.1 ) THEN
 
            ximin = EXP(-Beta*Pthigh)
            xidel = 1.D0 - ximin
 40         Pts = -LOG((xidel*DT_RNDM(xidel)+ximin)
     &            *(xidel*DT_RNDM(ximin)+ximin)+DEPS)/Beta
            IF ( Pts.LT.xmt ) GOTO 40
            Pts = SQRT(Pts**2-xmt2)
            IF ( (Pts.GT.Pthigh) .OR. (Pts.LT.Ptlow) ) GOTO 40
 
         ELSE IF ( Imode.EQ.2 ) THEN
 
            IF ( Ee.GE.0.D0 ) THEN
               p2 = PVIrtp(1)
            ELSE
               p2 = PVIrtp(2)
            END IF
            xv = ABS(Ee)
            aa = (1.D0-xv)*xv*p2 + PARmdl(25)
 60         Pts = SQRT(aa/(DT_RNDM(px)+EPS)-aa)
            IF ( (Pts.GT.Pthigh) .OR. (Pts.LT.Ptlow) ) GOTO 60
 
C  something wrong
 
         ELSE IF ( Imode.NE.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I4)')
     &            'PHO_SELPT:ERROR: invalid IMODE' , Imode
            CALL PHO_ABORT
         END IF
      END IF
 
C  debug output
      IF ( IDEb(5).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,4E10.3)')
     &         'PHO_SELPT: MODE,BET,PTMI,PTMA,PT' , Imode , Beta , 
     &        Ptlow , Pthigh , Pts
      END IF
 
99999 END SUBROUTINE
