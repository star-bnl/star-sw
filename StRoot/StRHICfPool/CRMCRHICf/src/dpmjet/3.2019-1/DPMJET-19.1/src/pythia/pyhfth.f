cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYHFTH
C...Gives threshold attractive/repulsive factor for heavy flavour
C...production.
 
      DOUBLE PRECISION FUNCTION PYHFTH(SH,SQM,FRATT)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pypars'
      include 'inc/pyint1'
 
C...Value for alpha_strong.
      IF(MSTP(35).LE.1) THEN
        ALSSG=PARP(35)
      ELSE
        MST115=MSTU(115)
        MSTU(115)=MSTP(36)
        Q2BN=SQRT(MAX(1D0,SQM*((SQRT(SH)-2D0*SQRT(SQM))**2+
     &  PARP(36)**2)))
        ALSSG=PYALPS(Q2BN)
        MSTU(115)=MST115
      ENDIF
 
C...Evaluate attractive and repulsive factors.
      XATTR=4D0*PARU(1)*ALSSG/(3D0*SQRT(MAX(1D-20,1D0-4D0*SQM/SH)))
      FATTR=XATTR/(1D0-EXP(-MIN(50D0,XATTR)))
      XREPU=PARU(1)*ALSSG/(6D0*SQRT(MAX(1D-20,1D0-4D0*SQM/SH)))
      FREPU=XREPU/(EXP(MIN(50D0,XREPU))-1D0)
      PYHFTH=FRATT*FATTR+(1D0-FRATT)*FREPU
      VINT(138)=PYHFTH
 
      RETURN
      END
