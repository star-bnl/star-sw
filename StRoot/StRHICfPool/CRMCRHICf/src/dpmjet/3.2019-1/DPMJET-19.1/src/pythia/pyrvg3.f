cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVG3
C...Function to do Y integration over true interference contributions
 
      DOUBLE PRECISION FUNCTION PYRVG3(X)
 
      IMPLICIT NONE
      include 'inc/pyrvpm'
C...Second Dalitz variable for PYRVG4
      COMMON/PYG2DX/X1
      DOUBLE PRECISION X, X1
      DOUBLE PRECISION E2, E3, C1, SQ1, SR1, SR2, YMIN, YMAX
      DOUBLE PRECISION PYRVG4, PYGAU2
      EXTERNAL PYGAU2,PYRVG4
      SAVE /PYG2DX/
      PYRVG3=0D0
      C1=2D0*SQRT(MAX(1D-9,X))
      X1=X
      IF (.NOT.MFLAG) THEN
        E2    = X/C1
        E3    = (RM(0)**2-X)/C1
        YMIN  = 0D0
        YMAX  = 4D0*E2*E3
      ELSE
        E2    = (X-RM(1)**2+RM(2)**2)/C1
        E3    = (RM(0)**2-X-RM(3)**2)/C1
        SQ1   = (E2+E3)**2
        SR1   = SQRT(MAX(0D0,E2**2-RM(2)**2))
        SR2   = SQRT(MAX(0D0,E3**2-RM(3)**2))
        YMIN  = SQ1-(SR1+SR2)**2
        YMAX  = SQ1-(SR1-SR2)**2
      ENDIF
      PYRVG3 = PYGAU2(PYRVG4,YMIN,YMAX,1D-3)
      RETURN
      END
