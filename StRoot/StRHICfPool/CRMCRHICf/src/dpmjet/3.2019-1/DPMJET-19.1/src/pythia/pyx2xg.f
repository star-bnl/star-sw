cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYX2XG
C...Calculates the decay rate for ino -> ino + gauge boson.
 
      DOUBLE PRECISION FUNCTION PYX2XG(C1,XM1,XM2,XM3,GX2,GLR)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Local variables.
      DOUBLE PRECISION XM1,XM2,XM3,GX2,GLR
      DOUBLE PRECISION XL,PYLAMF,C1
      DOUBLE PRECISION XMI2,XMJ2,XMV2,XMI3
 
      XMI2=XM1**2
      XMI3=ABS(XM1**3)
      XMJ2=XM2**2
      XMV2=XM3**2
      XL=PYLAMF(XMI2,XMJ2,XMV2)
      PYX2XG=C1/8D0/XMI3*SQRT(XL)
     &*(GX2*(XL+3D0*XMV2*(XMI2+XMJ2-XMV2))-
     &12D0*GLR*XM1*XM2*XMV2)
 
      RETURN
      END
