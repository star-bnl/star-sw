cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYXXGA
C...Calculates chi0_i -> chi0_j + gamma.
 
      DOUBLE PRECISION FUNCTION PYXXGA(C0,XM1,XM2,XMTR,XMTL)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Local variables.
      DOUBLE PRECISION C0,XM1,XM2,XMTR,XMTL
      DOUBLE PRECISION F1,F2
 
      F1=(1D0+XMTR/(1D0-XMTR)*LOG(XMTR))/(1D0-XMTR)
      F2=(1D0+XMTL/(1D0-XMTL)*LOG(XMTL))/(1D0-XMTL)
      PYXXGA=C0*((XM1**2-XM2**2)/XM1)**3
      PYXXGA=PYXXGA*(2D0/3D0*(F1+F2)-13D0/12D0)**2
 
      RETURN
      END
