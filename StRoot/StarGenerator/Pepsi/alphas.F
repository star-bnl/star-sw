      FUNCTION ALPHAS (Q2, NAORD)
*********************************************************************
*                                                                   *
*   THE ALPHA_S ROUTINE.                                            *
*                                                                   *
*   INPUT :  Q2    =  scale in GeV**2  (not too low, of course);    *
*            NAORD =  1 (LO),  2 (NLO).                             *
*                                                                   *
*   OUTPUT:  alphas_s/(4 pi) for use with the GRV(98) partons.      *  
*                                                                   *
*******************************************************i*************
*23456
      IMPLICIT DOUBLE PRECISION (A - Z)
      INTEGER NF, K, I, NAORD
      DIMENSION LAMBDAL (3:6),  LAMBDAN (3:6), Q2THR (3)
*
*...HEAVY QUARK THRESHOLDS AND LAMBDA VALUES :
      DATA Q2THR   /  1.960,  20.25,  30625. /
      DATA LAMBDAL / 0.2041, 0.1750, 0.1320, 0.0665 /
      DATA LAMBDAN / 0.2994, 0.2460, 0.1677, 0.0678 /
*
*...DETERMINATION OF THE APPROPRIATE NUMBER OF FLAVOURS :
      NF = 3
      DO 10 K = 1, 3
      IF (Q2 .GT. Q2THR (K)) THEN
         NF = NF + 1
      ELSE
          GO TO 20
       END IF
  10   CONTINUE
*
*...LO ALPHA_S AND BETA FUNCTION FOR NLO CALCULATION :
  20   B0 = 11.- 2./3.* NF
       B1 = 102.- 38./3.* NF
       B10 = B1 / (B0*B0)
       IF (NAORD .EQ. 1) THEN
         LAM2 = LAMBDAL (NF) * LAMBDAL (NF)
         ALP  = 1./(B0 * DLOG (Q2/LAM2))
         GO TO 1
       ELSE IF (NAORD .EQ. 2) then
         LAM2 = LAMBDAN (NF) * LAMBDAN (NF)
         B1 = 102.- 38./3.* NF
         B10 = B1 / (B0*B0)
       ELSE
         WRITE (6,91)
  91     FORMAT ('INVALID CHOICE FOR ORDER IN ALPHA_S')
         STOP
       END IF
*
*...START VALUE FOR NLO ITERATION :
       LQ2 = DLOG (Q2 / LAM2)
       ALP = 1./(B0*LQ2) * (1.- B10*DLOG(LQ2)/LQ2)
*
*...EXACT NLO VALUE, FOUND VIA NEWTON PROCEDURE :
       DO 2 I = 1, 6
       XL  = DLOG (1./(B0*ALP) + B10)
       XLP = DLOG (1./(B0*ALP*1.01) + B10)
       XLM = DLOG (1./(B0*ALP*0.99) + B10)
       Y  = LQ2 - 1./ (B0*ALP) + B10 * XL
       Y1 = (- 1./ (B0*ALP*1.01) + B10 * XLP
     1       + 1./ (B0*ALP*0.99) - B10 * XLP) / (0.02D0*ALP)
       ALP = ALP - Y/Y1
  2    CONTINUE
*
*...OUTPUT :
  1    ALPHAS = ALP
       RETURN
       END
