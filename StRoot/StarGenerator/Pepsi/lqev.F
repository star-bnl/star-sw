
C **********************************************************************

      SUBROUTINE LQEV

      IMPLICIT NONE

C...Generate an ordinary 2-jet event, q-event.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      
      INTEGER IFL,IFLR,IFLRAR,IFLRO,NREMH,K2,IFLAR
      REAL W,AMIFL,XT,AMK2,AMIFLR,PT,PHI,PT2,TM2K2,EK2,PZK2,EPZ,WT,
     +TMIFLR,EIFL,EIFLR,THER,THEK2
      REAL ULMASS,ULANGL
      INTEGER KFIFLR,LUCOMP,KFK2

      LST(24)=1
      W=SQRT(W2)

C...Choose flavour of scattered quark and target remnant.
  200 CALL LFLAV(IFL,IFLR)
      IF(LST(21).NE.0) GOTO 200

      GOTO 210
C...Entry used for Ariadne
      ENTRY LQEVAR(IFLAR,IFLRAR)
      IFL=IFLAR
      IFLR=IFLRAR
      LST(24)=1
      W=SQRT(W2)

  210 CONTINUE
      MSTJ(93)=1
      AMIFL=ULMASS(IFL)
      MSTJ(93)=1
      AMIFLR=ULMASS(IFLR)
      IF(LST(14).EQ.0.OR.IFLR.GT.10
     &.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
C...Check if energy in jet system is enough for fragmentation.
        IF(W.LT.AMIFL+AMIFLR+PARJ(32)) GOTO 200
        CALL LU2ENT(MSTU(1),IFL,IFLR,W)
        K(MSTU(1)+1,3)=2
      ELSE
C...Target remnant is not a simple diquark, special treatment needed.
        IF(W.LT.AMIFL+AMIFLR+0.9+PARJ(32)) GOTO 200
        IFLRO=IFLR
        NREMH=0
  300   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
C...Give balancing pt to IFLQ and IFLQQ.
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(IFLRO,PT,IFLR,K2,XT)
        MSTJ(93)=1
        AMIFLR=ULMASS(IFLR)
CJR--
        KFIFLR=LUCOMP(IFLR)
        IF (KFIFLR.EQ.90) THEN
          AMIFLR=AMIFLR-2.*PARL(20)
        ELSEIF (1.LE.KFIFLR .AND. KFIFLR.LE.6) THEN
          AMIFLR=AMIFLR-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMK2=ULMASS(K2)
        KFK2=LUCOMP(K2)
        IF (KFK2.EQ.90) THEN
          AMK2=AMK2-2.*PARL(20)
        ELSEIF (1.LE.KFK2 .AND. KFK2.LE.6) THEN
          AMK2=AMK2-PARL(20)
        ENDIF
CJR--
        PT2=PT**2
        TM2K2=AMK2**2+PT2
        EK2=.5*(XT*W+TM2K2/XT/W)
        PZK2=-.5*(XT*W-TM2K2/XT/W)
        EPZ=W-TM2K2/XT/W
        WT=(1.-XT)*W*EPZ-PT2
C...Check if energy in jet system is enough for fragmentation.
        IF(WT.LT.(AMIFL+AMIFLR+PARJ(32))**2) GOTO 300
        WT=SQRT(WT+PT2)
        TMIFLR=AMIFLR**2+PT2
        EIFL=.5*(WT+(AMIFL**2-TMIFLR)/WT)
        EIFLR=.5*(WT+(-AMIFL**2+TMIFLR)/WT)
        THER=ULANGL(-SQRT(EIFLR**2-TMIFLR),PT)
C...Form jet system.
C...Use the same mass as above to avoid momentum non-conservation
        MSTU(10)=1
        P(MSTU(1),5)=AMIFL
        CALL LU1ENT(-MSTU(1),IFL,EIFL,0.,0.)
        MSTU(10)=1
        P(MSTU(1)+1,5)=AMIFLR
        CALL LU1ENT(MSTU(1)+1,IFLR,EIFLR,THER,PHI)
        CALL LUDBRB(MSTU(1),0,0.,0.,0.D0,0.D0,
     &  (DBLE(EPZ)-(1.D0-DBLE(XT))*DBLE(W))/
     &  (DBLE(EPZ)+(1.D0-DBLE(XT))*DBLE(W)))
        THEK2=ULANGL(PZK2,PT)
C...Add formed "target" particle.
        MSTU(10)=1
        P(MSTU(1)+2,5)=AMK2
        CALL LU1ENT(MSTU(1)+2,K2,EK2,THEK2,PHI+3.1415927)
        MSTU(10)=2
        K(MSTU(1)+1,3)=2
        K(MSTU(1)+2,3)=2
CIC...Target remnants required to go backwards in hadronic cms
        IF(P(MSTU(1)+1,3).GT.0..OR.P(MSTU(1)+2,3).GT.0.) GOTO 300
      ENDIF
      
CAE...Set reasonable values to the ME variables xp,zq and phi
      PARL(28)=1.0
      PARL(29)=1.0
      PARL(30)=0.0

      LST(21)=0
      RETURN

  999 LST(21)=3
      RETURN
      END
