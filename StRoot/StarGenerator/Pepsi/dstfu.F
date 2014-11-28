************************************************************************
      SUBROUTINE DSTFU(KF,X,Q2,XDPQ)

       IMPLICIT NONE

C...Gives proton and neutron structure functions according to a few
C...different polarized parametrizations.
C...Note that what is coded is x times the probability distribution,
C...i.e. xdq(x,Q2) etc.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /ARSTRF/ KFSAVE(2),XSAVE(2),XQ2SAV(2),
     +        XPQSAV(2,-6:6),XDPQSAV(2,-6:6)
      INTEGER KFSAVE
      REAL XSAVE,XQ2SAV,XPQSAV,XDPQSAV
      SAVE /ARSTRF/


      INTEGER KF,KFL,KFA
      REAL X,Q2,XPQ,XDPQ,XDPS
      DIMENSION XPQ(-6:6),XDPQ(-6:6)

C...Reset structure functions.
      DO 100 KFL=-6,6
      XDPQ(KFL)=0.
 100  XDPQSAV(1,KFL)=0.
      XSAVE(1)=X
      XQ2SAV(1)=Q2
      KFSAVE(1)=KF

C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(6,5000) X
        RETURN
      ENDIF
      KFA=IABS(KF)
      IF(KFA.NE.2112.AND.KFA.NE.2212) THEN
       WRITE(6,5100) KF
       RETURN
      ENDIF

*...  Put into output array  
      CALL PARTON(X,Q2,XPQ,XDPQ)


C...Check positivity and reset above maximum allowed flavour.
      DO 180 KFL=-6,6
      IF(IABS(KFL).GT.LST(12)) THEN 
        XDPQ(KFL)=0.
      ENDIF
 180  CONTINUE

C...  Isospin conjugation for neutron.
*HI>>      
      IF(KFA.EQ.2112.AND.LST(39).EQ.0) THEN
*     IF(KFA.EQ.2112) THEN
        XDPS = XDPQ(1)
        XDPQ(1) = XDPQ(2)
        XDPQ(2) = XDPS
        XDPS = XDPQ(-1)
        XDPQ(-1) = XDPQ(-2)
        XDPQ(-2) = XDPS
      ENDIF

C...Charge conjugation for antiparticle.
      IF(KF.LT.0) THEN
        DO 170 KFL=1,6
        XDPS = XDPQ(KFL)
        XDPQ(KFL) = XDPQ(-KFL)
        XDPQ(-KFL) = XDPS
  170   CONTINUE
      ENDIF

      DO 120 KFL=-6,6
  120 XDPQSAV(1,KFL)=XDPQ(KFL)

C...Formats for error printouts.
 5000 FORMAT(' Error: x value outside physical range; x =',1P,E12.3)
 5100 FORMAT(' Error: illegal particle code for structure function;',
     &' KF =',I5)
 5200 FORMAT(' Error: unknown structure function; KF, library, set =',
     &3I5)

      RETURN
      END
