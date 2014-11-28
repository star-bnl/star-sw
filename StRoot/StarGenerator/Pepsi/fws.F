*
       FUNCTION FWS (X, S, AL, BE, AK, AG, B, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = DSQRT (X)
       LX = DLOG (1./X)
       FWS = S**AL / LX**AK * (1.+ AG*DX + B*X) * (1.- X)**D
     1       * DEXP (-E + DSQRT (ES * S**BE * LX))
       RETURN
       END
