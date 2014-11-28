*
*...FUNCTIONAL FORMS OF THE PARAMETRIZATIONS :
*
       FUNCTION FV (X, N, AK, BK, A, B, C, D)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = DSQRT (X)
       FV = N * X**AK * (1.+ A*X**BK + X * (B + C*DX)) * (1.- X)**D
       RETURN
       END
