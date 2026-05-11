cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PY1ENT
C...Stores one parton/particle in commonblock PYJETS.
 
      SUBROUTINE PY1ENT(IP,KF,PE,THE,PHI)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYCOMP

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pydat2'
 
C...Standard checks.
      MSTU(28)=0
      IF(MSTU(12).NE.12345) CALL PYLIST(0)
      IPA=MAX(1,ABS(IP))
      IF(IPA.GT.MSTU(4)) CALL PYERRM(21,
     &'(PY1ENT:) writing outside PYJETS memory')
      KC=PYCOMP(KF)
      IF(KC.EQ.0) CALL PYERRM(12,'(PY1ENT:) unknown flavour code')
 
C...Find mass. Reset K, P and V vectors.
      PM=0D0
      IF(MSTU(10).EQ.1) PM=P(IPA,5)
      IF(MSTU(10).GE.2) PM=PYMASS(KF)
      DO 100 J=1,5
        K(IPA,J)=0
        P(IPA,J)=0D0
        V(IPA,J)=0D0
  100 CONTINUE
 
C...Store parton/particle in K and P vectors.
      K(IPA,1)=1
      IF(IP.LT.0) K(IPA,1)=2
      K(IPA,2)=KF
      P(IPA,5)=PM
      P(IPA,4)=MAX(PE,PM)
      PA=SQRT(P(IPA,4)**2-P(IPA,5)**2)
      P(IPA,1)=PA*SIN(THE)*COS(PHI)
      P(IPA,2)=PA*SIN(THE)*SIN(PHI)
      P(IPA,3)=PA*COS(THE)
 
C...Set N. Optionally fragment/decay.
      N=IPA
      IF(IP.EQ.0) CALL PYEXEC
 
      RETURN
      END
