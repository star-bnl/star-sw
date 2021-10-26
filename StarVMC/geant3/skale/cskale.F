* $Id: cskale.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CSKALE(IBERT,F,NOFAS,ITSLO,EFAS,ALPFAS,BETFAS,GAMFAS,
     +                 EHICUT,RMFAS,EXFAS,REFAS)
C
CZ  modified SKALE -- generate WT particles
C
      DIMENSION  F(*)     , ITSLO(*) , EFAS(*) , ALPFAS(*) , BETFAS(*) ,
     +           GAMFAS(*), WTFAS(200), ESLO(200), ALPSLO(200),
     +           BETSLO(200), GAMSLO(200), NPI(200)
C
      LOGICAL FTROBL
C
      DATA IELAS/1/
      SAVE
C
      NKEY=2
      NCT=1
   10 ESTOR=F(3)
#if defined(ATLAS_CDEBUG)
      PRINT *,' CSKALE : EKIN=',ESTOR
#endif
      F(3)=EHICUT
C
      CALL CABERT(IBERT,F,NOSLO,ITSLO,ESLO,ALPSLO,BETSLO,GAMSLO)
      IBERT=1
      F(3)=ESTOR
C
      IF(NOSLO.GT.0) GO TO 70
      NOFAS=NOSLO
   20 CONTINUE
C
C scaling within geant
C generate N particles with RND*WT*Ei energy
C check for depletion of nucleus
C
      IF(NOFAS.LE.0) RETURN
C first determine recoil nucleus parameters
      FTROBL = .FALSE.
   30 CONTINUE
      AR=F(1)
      ZR=F(2)
      PI0NO = 0.0
      PIPNO = 0.0
      PIMNO = 0.0
      PRONO = 0.0
      XNEUT = 0.0
      DO 40 I=1,NOFAS
         IF(ITSLO(I).LE.0) PRONO = PRONO + 1.0
         IF(ITSLO(I).LE.1) XNEUT = XNEUT + 1.0
         IF(ITSLO(I).EQ.2) THEN
           CALL GRNDM(R,1)
           NPI(I) = INT(WTFAS(I))
           XN = WTFAS(I) - FLOAT(NPI(I)) - R
           IF(XN.GT.0.0) NPI(I) = NPI(I) + 1
           IF(NPI(I).LE.0.OR.FTROBL) NPI(I) = 1
           PIPNO = PIPNO + FLOAT(NPI(I))
         ELSE IF(ITSLO(I).EQ.4) THEN
           CALL GRNDM(R,1)
           NPI(I) = INT(WTFAS(I))
           XN = WTFAS(I) - FLOAT(NPI(I)) - R
           IF(XN.GT.0.0) NPI(I) = NPI(I) + 1
           IF(NPI(I).LE.0.OR.FTROBL) NPI(I) = 1
           PIMNO = PIMNO + FLOAT(NPI(I))
         ELSE IF(ITSLO(I).EQ.3) THEN
           CALL GRNDM(R,1)
           NPI(I) = INT(WTFAS(I))
           XN = WTFAS(I) - FLOAT(NPI(I)) - R
           IF(XN.GT.0.0) NPI(I) = NPI(I) + 1
           IF(NPI(I).LE.0.OR.FTROBL) NPI(I) = 1
         ENDIF
   40 CONTINUE
      AADD = 1.
      IF(F(7).GT.1) AADD = 0.0
      ZADD = 1. - F(7)
      IF(F(7).GT.1) ZADD = 3.-F(7)
      AR = AR + AADD - PRONO - XNEUT
      ZR = ZR + ZADD - PRONO - PIPNO + PIMNO
      IF((ZR.LT.0.OR.AR.LT.ZR).AND..NOT.FTROBL) THEN
        FTROBL = .TRUE.
        GOTO 30
      ENDIF
C  start generating more particles
      K = NOFAS + 1
C loop over particles
      DO 60 I=1,NOFAS
C don't scale nucleons
         IF(ITSLO(I).LE.1) THEN
            EFAS(I) = EFAS(I)*WTFAS(I)
         ELSE
          DO 50 J=1,NPI(I)
            IF(J.EQ.NPI(I)) THEN
              IF(WTFAS(I).GT.0.0) EFAS(I) = EFAS(I)*WTFAS(I)
            ELSE
              WTFAS(I) = WTFAS(I) - 1.
              EFAS(K) = EFAS(I)
              ITSLO(K)  = ITSLO(I)
              ALPFAS(K) = ALPFAS(I)
              BETFAS(K) = BETFAS(I)
              GAMFAS(K) = GAMFAS(I)
              K = K + 1
            ENDIF
   50     CONTINUE
         ENDIF
   60 CONTINUE
      NOFAS=K-1
      RETURN
   70 IF(IELAS.EQ.0) GO TO 80
      CALL ESKALE(IE,EHICUT,F,NOFAS,ITSLO,EFAS,ALPFAS,BETFAS,GAMFAS,
     + WTFAS,RMFAS,EXFAS,REFAS,NOSLO,ESLO,ALPSLO,BETSLO,GAMSLO)
      GO TO (80,10,20),IE
   80 ATAR=F(1)
      EINC=F(3)
      ITINC=F(7)
      CALL MCMOSC(NKEY,ATAR,ITINC,EINC,EHICUT,NOSLO,ITSLO,ESLO,ALPSLO,
     + BETSLO,GAMSLO,EFAS,WTFAS,ALPFAS,BETFAS,GAMFAS,RMFAS,NOFAS,EXFAS,
     + REFAS,WHY)
      IF(NOFAS.EQ.0) GO TO 10
      GOTO 20
      END
