cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVSB
C...Auxiliary function to PYRVSF for calculating R-Violating
C...sfermion widths. Though the decay products are most often treated
C...as massless in the calculation, the kinematical boundary of phase
C...space is tested using the true masses.
C...MODE = 1: All decay products massive
C...MODE = 2: Decay product 1 massless
C...MODE = 3: Decay product 2 massless
C...MODE = 4: All decay products  massless
 
      DOUBLE PRECISION FUNCTION PYRVSB(KFIN,ID1,ID2,RM2,MODE)
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      include 'inc/pydat1'
      include 'inc/pydat2'
      DOUBLE PRECISION SM(3)
      INTEGER PYCOMP, KC(3)
      KC(1)=PYCOMP(KFIN)
      KC(2)=PYCOMP(ID1)
      KC(3)=PYCOMP(ID2)
      SM(1)=PMAS(KC(1),1)**2
      SM(2)=PMAS(KC(2),1)**2
      SM(3)=PMAS(KC(3),1)**2
C...Kinematics check
      IF ((SM(1)-(PMAS(KC(2),1)+PMAS(KC(3),1))**2).LE.0D0) THEN
        PYRVSB=0D0
        RETURN
      ENDIF
C...CM momenta squared
      IF (MODE.EQ.1) THEN
        P2CM=1./(4*SM(1))*(SM(1)-(PMAS(KC(2),1)+PMAS(KC(3),1))**2)
     &       * (SM(1)-(PMAS(KC(2),1)-PMAS(KC(3),1))**2)
      ELSE IF (MODE.EQ.2) THEN
        P2CM=1./(4*SM(1))*(SM(1)-(PMAS(KC(3),1))**2)**2
      ELSE IF (MODE.EQ.3) THEN
        P2CM=1./(4*SM(1))*(SM(1)-(PMAS(KC(2),1))**2)**2
      ELSE
        P2CM=SM(1)/4.
      ENDIF
C...Calculate Width
      PYRVSB=RM2*SQRT(MAX(0D0,P2CM))/(8*PARU(1)*SM(1))
      RETURN
      END
