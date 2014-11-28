*
       FUNCTION FW (X, S, AL, BE, AK, BK, A, B, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       LX = DLOG (1./X)
       FW = (X**AK * (A + X * (B + X*C)) * LX**BK + S**AL
     1      * DEXP (-E + DSQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
