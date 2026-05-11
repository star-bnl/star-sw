cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVG4
C...Integrand for true intereference contributions
 
      DOUBLE PRECISION FUNCTION PYRVG4(Y)
 
      IMPLICIT NONE
      include 'inc/pyrvpm'
      COMMON/PYG2DX/X
      DOUBLE PRECISION X, Y, RVS, PYRVS
      SAVE /PYG2DX/
      PYRVG4=0D0
      RVS=PYRVS(X,Y,RESM(1),RESW(1),RESM(2),RESW(2))
      IF (.NOT.MFLAG) THEN
        PYRVG4 = RVS*B(1)*B(2)*X*Y
      ELSE
        PYRVG4 = RVS*(RM(1)*RM(3)*A(1)*A(2)*(X+Y-RM(1)**2-RM(3)**2)
     &       + RM(1)*RM(0)*B(1)*A(2)*(Y-RM(2)**2-RM(3)**2)
     &       + RM(3)*RM(0)*A(1)*B(2)*(X-RM(1)**2-RM(2)**2)
     &       + B(1)*B(2)*(X*Y-(RM(1)*RM(3))**2-(RM(0)*RM(2))**2))
      ENDIF
      RETURN
      END
