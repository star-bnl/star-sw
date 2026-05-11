cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYMEWT
C...Calculates actual ME weight in some initial-state showers.
C...Inparameter MECOR: kind of hard scattering process
C...            IFLCB: flavour combination of branching,
C...                   1 for fermion -> fermion,
C...                   2 for gluon/photon -> fermion
C...                   3 for fermion -> gluon/photon,
C...                   4 for gluon -> gluon
C...            Q2:    Q2 value of shower branching
C...            Z:     Z value of branching
C...In+outparameter PHIBR: azimuthal angle of branching
C...Outparameter WTME: actual ME weight
 
      SUBROUTINE PYMEWT(MECOR,IFLCB,Q2,Z,PHIBR,WTME)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pypars'
      include 'inc/pyint1'
      include 'inc/pyint2'
 
C...Default output.
      WTME=1D0
 
C...Define kinematics of shower branching in Mandelstam variables.
      SQM=VINT(44)
      SH=SQM/Z
      TH=-Q2
      UH=Q2-SQM*(1D0-Z)/Z
 
C...Matrix-element corrections for f + fbar -> s-channel vector boson.
      IF(MECOR.EQ.1) THEN
        IF(IFLCB.EQ.1) THEN
          WTME=(TH**2+UH**2+2D0*SQM*SH)/(SH**2+SQM**2)
        ELSEIF(IFLCB.EQ.2) THEN
          WTME=(SH**2+TH**2+2D0*SQM*UH)/((SH-SQM)**2+SQM**2)
        ENDIF
 
C...Matrix-element corrections for g + g -> Higgs (h0, H0, A0).
      ELSEIF(MECOR.EQ.2) THEN
        IF(IFLCB.EQ.3) THEN
          WTME=(SH**2+UH**2)/(SH**2+(SH-SQM)**2)
        ELSEIF(IFLCB.EQ.4) THEN
          WTME=0.5D0*(SH**4+UH**4+TH**4+SQM**4)/(SH**2-SQM*(SH-SQM))**2
        ENDIF

C...Matrix-element corrections for q + qbar -> Higgs (h0)
      ELSEIF(MECOR.EQ.3) THEN
        IF(IFLCB.EQ.2) THEN
          WTME=(SH**2+TH**2+2D0*(SQM-TH)*(SQM-SH))/
     1      (SH**2+2D0*SQM*(SQM-SH))
        ENDIF
      ENDIF
 
      RETURN
      END
