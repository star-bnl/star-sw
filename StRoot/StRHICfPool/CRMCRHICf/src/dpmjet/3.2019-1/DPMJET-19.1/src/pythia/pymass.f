cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYMASS
C...Gives the mass of a particle/parton.
 
      DOUBLE PRECISION FUNCTION PYMASS(KF)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYCOMP

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pydat2'
 
C...Reset variables. Compressed code. Special case for popcorn diquarks.
      PYMASS=0D0
      KFA=ABS(KF)
      KC=PYCOMP(KF)
      IF(KC.EQ.0) THEN
        MSTJ(93)=0
        RETURN
      ENDIF
 
C...Guarantee use of constituent masses for internal checks.
      IF((MSTJ(93).EQ.1.OR.MSTJ(93).EQ.2).AND.
     &(KFA.LE.10.OR.MOD(KFA/10,10).EQ.0)) THEN
        IF(KFA.LE.5) THEN
          PYMASS=PARF(100+KFA)
          IF(MSTJ(93).EQ.2) PYMASS=MAX(0D0,PYMASS-PARF(121))
        ELSEIF(KFA.LE.10) THEN
          PYMASS=PMAS(KFA,1)
        ELSEIF(MSTJ(93).EQ.1) THEN
          PYMASS=PARF(100+MOD(KFA/1000,10))+PARF(100+MOD(KFA/100,10))
        ELSE
          PYMASS=MAX(0D0,PMAS(KC,1)-PARF(122)-2D0*PARF(112)/3D0)
        ENDIF
 
C...Other masses can be read directly off table.
      ELSE
        PYMASS=PMAS(KC,1)
      ENDIF
 
C...Optional mass broadening according to truncated Breit-Wigner
C...(either in m or in m^2).
      IF(MSTJ(24).GE.1.AND.PMAS(KC,2).GT.1D-4) THEN
        IF(MSTJ(24).EQ.1.OR.(MSTJ(24).EQ.2.AND.KFA.GT.100)) THEN
          PYMASS=PYMASS+0.5D0*PMAS(KC,2)*TAN((2D0*PYR(0)-1D0)*
     &    ATAN(2D0*PMAS(KC,3)/PMAS(KC,2)))
        ELSE
          PM0=PYMASS
          PMLOW=ATAN((MAX(0D0,PM0-PMAS(KC,3))**2-PM0**2)/
     &    (PM0*PMAS(KC,2)))
          PMUPP=ATAN(((PM0+PMAS(KC,3))**2-PM0**2)/(PM0*PMAS(KC,2)))
          PYMASS=SQRT(MAX(0D0,PM0**2+PM0*PMAS(KC,2)*TAN(PMLOW+
     &    (PMUPP-PMLOW)*PYR(0))))
        ENDIF
      ENDIF
      MSTJ(93)=0
 
      RETURN
      END
