C **********************************************************************
#define RLU(x) RLUSTAR(x)
      SUBROUTINE LFLAV(IFL,IFLR)

      IMPLICIT NONE

C...Choose flavour of struck quark and the
C...corresponding flavour of the target remnant jet.

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

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /LFLMIX/ CABIBO(4,4)
      REAL CABIBO
      DIMENSION PDF(-6:6)

      INTEGER IFL,IFLR,NFL,I,IFLA,J1,M1,M2,J,IFLRA,J2
      REAL R,PSUB,AMIFL,AMIFLR,AMU,ULMASS,RLU,USEA,DSEA,PDF,RLUSTAR

      LST(21)=0
      IF(LST(24).EQ.3) THEN
        NFL=LST(13)
      ELSE
        NFL=LST(12)
      ENDIF

   20 R=RLU(0)*PQ(17)
      PSUB=0.
      DO 30 I=1,2*NFL
      IFL=I
      PSUB=PSUB+PQ(I)
      IF(R.LE.PSUB) GOTO 40
   30 CONTINUE
   40 CONTINUE
      IF(IFL.GT.NFL) IFL=NFL-IFL
      LST(25)=IFL
      IFLR=-IFL

      IF(LST(23).EQ.2) THEN
C...Weak charged current, change the flavour of the struck
C...quark using generalized Cabibbo mixing matrix.
        IFLA=IABS(IFL)
        J1=(IFLA+1)/2
        M1=MOD(IFLA,2)
        M2=MOD(IFLA+1,2)
        R=RLU(0)
        PSUB=0.
        DO 100 J=1,4
        J2=J
        PSUB=PSUB+CABIBO(M1*J2+M2*J1,M2*J2+M1*J1)
        IF(R.LT.PSUB) GOTO 200
  100   CONTINUE
  200   IFL=2*J2-M2
        IF(LST(25).LT.0) IFL=-IFL
      ENDIF

      IFLA=IABS(IFL)
      IFLRA=IABS(IFLR)
      IF(IFLA.GE.4.OR.IFLRA.GE.4) THEN
C...Threshold function for heavy quarks of flavour IFLA and IFLRA.
        MSTJ(93)=1
        AMU=ULMASS(1)
        MSTJ(93)=1
        AMIFL=ULMASS(IFLA)
        MSTJ(93)=1
        AMIFLR=ULMASS(IFLRA)
        IF(1.-(.938+AMIFL+AMIFLR+2.*AMU)**2/W2.LT.RLU(0))
     &  GOTO(20,999,999) LST(24)
      ENDIF

C...Remnant flavour taken care of later for qqbar event and ME+PS case
      IF(LST(24).EQ.3) RETURN
      IF(LST(8).GT.10.AND.MOD(LST(8),10).NE.9) RETURN

C...With LST(14)=0/1(default) baryon production from the target remnant
C...is excluded/included.
      IF(LST(14).EQ.0) RETURN
C-GI-021119...For u and d quarks, choose if valence or sea quark.
      CALL LYSTFU(2212,X,Q2,PDF)
      USEA=0.
      DSEA=0.
      IF(PDF(2).GT.0.) USEA=PDF(-2)/PDF(2)
      IF(PDF(1).GT.0.) DSEA=PDF(-1)/PDF(1)
      IF(IFLR.EQ.-2) THEN
        IF(LST(22).EQ.1.AND.RLU(0).GT.USEA) THEN
          IFLR=2101
          IF(RLU(0).GT.PARL(4)) IFLR=2103
        ELSEIF(LST(22).EQ.2.AND.RLU(0).GT.DSEA) THEN
          IFLR=1103
        ENDIF
      ELSEIF(IFLR.EQ.-1) THEN
        IF(LST(22).EQ.1.AND.RLU(0).GT.DSEA) THEN
          IFLR=2203
        ELSEIF(LST(22).EQ.2.AND.RLU(0).GT.USEA) THEN
          IFLR=2101
          IF(RLU(0).GT.PARL(4)) IFLR=2103
        ENDIF
      ENDIF
      RETURN

  999 LST(21)=6
      RETURN
      END

C **********************************************************************

