cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYH2XX
C...Calculates the decay rate for a Higgs to an ino pair.
 
      DOUBLE PRECISION FUNCTION PYH2XX(C1,XM1,XM2,XM3,GX2,GLR)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
 
C...Local variables.
C unvar      DOUBLE PRECISION XM1,XM2,XM3,GL,GR
      DOUBLE PRECISION XM1,XM2,XM3
      DOUBLE PRECISION XL,PYLAMF,C1
      DOUBLE PRECISION XMI2,XMJ2,XMK2,XMI3
 
      XMI2=XM1**2
      XMI3=ABS(XM1**3)
      XMJ2=XM2**2
      XMK2=XM3**2
      XL=PYLAMF(XMI2,XMJ2,XMK2)
      PYH2XX=C1/4D0/XMI3*SQRT(XL)
     &*(GX2*(XMI2-XMJ2-XMK2)-
     &4D0*GLR*XM3*XM2)
      IF(PYH2XX.LT.0D0) PYH2XX=0D0
 
      RETURN
      END
