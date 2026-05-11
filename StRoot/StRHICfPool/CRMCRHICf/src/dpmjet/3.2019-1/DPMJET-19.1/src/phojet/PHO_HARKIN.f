
      SUBROUTINE PHO_HARKIN(Irej)
C***********************************************************************
C
C     selection of kinematic variables
C     (resolved and direct processes)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , r , rm , TINY , TINYP , wl
      INTEGER Irej , m , MAX_PRO_2 , mm
      SAVE 
 
      PARAMETER (TINY=1.D-30,TINYP=1.D-14)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  internal cross check information on hard scattering limits
      INCLUDE 'inc/pohlim'
 
      PARAMETER (MAX_PRO_2=16)
      DIMENSION rm(-1:MAX_PRO_2)
      DATA rm/3.31D0 , 0.0D0 , 7.60D0 , 0.65D0 , 4.00D0 , 0.65D0 , 
     &     0.89D0 , 0.45D0 , 0.89D0 , 0.89D0 , 0.0D0 , 4.776D0 , 
     &     0.615D0 , 4.776D0 , 0.615D0 , 1.0D0 , 0.0D0 , 1.0D0/
 
      Irej = 0
      m = MSPr
 
C------------- resolved processes -----------
      IF ( m.EQ.1 ) THEN
 50      CALL PHO_HARX12
         V = -0.5D0*W1/(W1+DT_RNDM(X1)*W)
         U = -1.D0 - V
         r = (1.D0+W)*2.25D0*(V*V*(3.D0-U*V-V/(U*U))-U)
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(1)*DT_RNDM(X2) ) GOTO 50
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
      ELSE IF ( m.EQ.2 .OR. m.EQ.4 ) THEN
 100     CALL PHO_HARX12
         wl = LOG(W1)
         V = -EXP(-0.6931472D0+DT_RNDM(X1)*wl)
         U = -1.D0 - V
         r = (U*U+V*V)*((16.D0/27.D0)/U-(4.D0/3.D0)*V)*(wl/W)*AXX
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(m)*DT_RNDM(X2) ) GOTO 100
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
      ELSE IF ( m.EQ.3 ) THEN
 150     CALL PHO_HARX12
         V = -0.5D0*W1/(W1+DT_RNDM(X1)*W)
         U = -1.D0 - V
         r = (1.D0+W)*(1.D0+U*U)*(1.D0-(4.D0/9.D0)*V*V/U)
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(3)*DT_RNDM(X2) ) GOTO 150
      ELSE IF ( m.EQ.5 ) THEN
 200     CALL PHO_HARX12
         V = -0.5D0*AXX/(W1+2.D0*DT_RNDM(X1)*W)
         U = -1.D0 - V
         r = (4.D0/9.D0)*(1.D0+U*U+V*V*(U*U+V*V)) - (8.D0/27.D0)*U*U*V
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(5)*DT_RNDM(X2) ) GOTO 200
      ELSE IF ( m.EQ.6 ) THEN
 250     CALL PHO_HARX12
         V = -0.5D0*(1.D0+W) + DT_RNDM(X1)*W
         U = -1.D0 - V
         r = (4.D0/9.D0)*(U*U+V*V)*AXX
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(6)*DT_RNDM(V) ) GOTO 250
      ELSE IF ( m.EQ.7 ) THEN
 300     CALL PHO_HARX12
         V = -0.5D0*W1/(W1+DT_RNDM(X1)*W)
         U = -1.D0 - V
         r = (1.D0+W)*((2.D0/9.D0)*(1.D0+U*U+(1.D0+V*V)*V*V/(U*U))
     &       -(4.D0/27.D0)*V/U)
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(7)*DT_RNDM(X2) ) GOTO 300
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
      ELSE IF ( m.EQ.8 ) THEN
 350     CALL PHO_HARX12
         V = -0.5D0*AXX/(W1+2.D0*DT_RNDM(X1)*W)
         U = -1.D0 - V
         r = (4.D0/9.D0)*(1.D0+U*U)
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(8)*DT_RNDM(X2) ) GOTO 350
      ELSE IF ( m.EQ.-1 ) THEN
 400     CALL PHO_HARX12
         wl = LOG(W1)
         V = -EXP(-0.6931472D0+DT_RNDM(X1)*wl)
         U = -1.D0 - V
         r = (1.D0+V*V)*(V/(U*U)-(4.D0/9.D0))*(wl/W)*AXX
         IF ( LPRi.GT.4 .AND. r*W.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r*W.LT.rm(-1)*DT_RNDM(X2) ) GOTO 400
C------------- direct / single-resolved processes -----------
      ELSE IF ( m.EQ.10 ) THEN
 450     CALL PHO_HARDX1
         wl = LOG(AXX/(1.D0+W)**2)
         U = -(1.D0+W)/2.D0*EXP(DT_RNDM(X1)*wl)
         r = -(8.D0/3.D0)*(U*U+1.D0)*wl*AXX
         IF ( LPRi.GT.4 .AND. r.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r.LT.rm(10)*DT_RNDM(U) ) GOTO 450
         V = -1.D0 - U
         X2 = X1
         X1 = 1.D0
      ELSE IF ( m.EQ.11 ) THEN
 500     CALL PHO_HARDX1
         wl = LOG(W1)
         U = -EXP(-0.6931472D0+DT_RNDM(X1)*wl)
         V = -1.D0 - U
         r = (U*U+V*V)/V*wl*AXX
         IF ( LPRi.GT.4 .AND. r.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r.LT.rm(11)*DT_RNDM(X2) ) GOTO 500
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
         X2 = X1
         X1 = 1.D0
      ELSE IF ( m.EQ.12 ) THEN
 550     CALL PHO_HARDX1
         wl = LOG(AXX/(1.D0+W)**2)
         V = -(1.D0+W)/2.D0*EXP(DT_RNDM(X1)*wl)
         r = -(8.D0/3.D0)*(V*V+1.D0)*wl*AXX
         IF ( LPRi.GT.4 .AND. r.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r.LT.rm(12)*DT_RNDM(V) ) GOTO 550
      ELSE IF ( m.EQ.13 ) THEN
 600     CALL PHO_HARDX1
         wl = LOG(W1)
         V = -EXP(-0.6931472D0+DT_RNDM(X1)*wl)
         U = -1.D0 - V
         r = (U*U+V*V)/U*wl*AXX
         IF ( LPRi.GT.4 .AND. r.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r.LT.rm(13)*DT_RNDM(X2) ) GOTO 600
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
C------------- (double) direct process -----------
      ELSE IF ( (m.EQ.14) .OR. (m.EQ.16) ) THEN
         X1 = 1.D0
         X2 = 1.D0
         AXX = AH
         W = SQRT(MAX(TINY,1.D0-AXX))
         W1 = AXX/(1.D0+W)
         wl = LOG(W1)
 650     V = -EXP(-0.6931472D0+DT_RNDM(X1)*wl)
         U = -1.D0 - V
         r = -(U*U+V*V)/U
         IF ( LPRi.GT.4 .AND. r.GT.rm(m) ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_HARKIN:weight error' , m
         IF ( r.LT.rm(14)*DT_RNDM(X2) ) GOTO 650
         IF ( DT_RNDM(V).LE.0.5D0 ) V = U
C---------------------------------------------
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3)')
     &         'PHO_HARKIN:ERROR:unsupported process (MSPR)' , MSPr
         CALL PHO_ABORT
      END IF
 
      V = MAX(MIN(V,-TINYP),-1.D0+TINYP)
      U = -1.D0 - V
      U = MAX(MIN(U,-TINYP),-1.D0+TINYP)
      PT = SQRT(U*V*X1*X2)*ECMp
      ETAc = 0.5D0*LOG((U*X1)/(V*X2))
      ETAd = 0.5D0*LOG((V*X1)/(U*X2))
 
C**************************************************************
      mm = m
      IF ( m.EQ.-1 ) mm = 3
      ETAmi(1,mm) = MIN(ETAmi(1,mm),ETAc)
      ETAma(1,mm) = MAX(ETAma(1,mm),ETAc)
      ETAmi(2,mm) = MIN(ETAmi(2,mm),ETAd)
      ETAma(2,mm) = MAX(ETAma(2,mm),ETAd)
      XXMi(1,mm) = MIN(XXMi(1,mm),X1)
      XXMa(1,mm) = MAX(XXMa(1,mm),X1)
      XXMi(2,mm) = MIN(XXMi(2,mm),X2)
      XXMa(2,mm) = MAX(XXMa(2,mm),X2)
C**************************************************************
 
      IF ( LPRi.GT.4 .AND. IDEb(81).GE.25 )
     &      WRITE (LO,'(1X,A,/5X,6E12.3)')
     &      'PHO_HARKIN: V,PT,ETAC,ETAD,X1,X2' , V , PT , ETAc , ETAd , 
     &     X1 , X2
 
      END SUBROUTINE
