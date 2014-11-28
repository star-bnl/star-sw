*
*...NLO PARAMETRIZATION (MS(BAR)) :
*
*72*********************************************************************      
      SUBROUTINE GRV94HO (X, Q2, UV, DV, DEL, UDB, SB, GL)

       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.34
       LAM2 = 0.248 * 0.248
       S  = DLOG (DLOG(Q2/LAM2) / DLOG(MU2/LAM2))
       DS = DSQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  1.304 + 0.863 * S 
       AKU =  0.558 - 0.020 * S
       BKU =          0.183 * S
       AU  = -0.113 + 0.283 * S - 0.321 * S2
       BU  =  6.843 - 5.089 * S + 2.647 * S2 - 0.527 * S3
       CU  =  7.771 - 10.09 * S + 2.630 * S2
       DU  =  3.315 + 1.145 * S - 0.583 * S2 + 0.154 * S3
       UV  = FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.102 - 0.017 * S + 0.005 * S2 
       AKD =  0.270 - 0.019 * S
       BKD =  0.260
       AD  =  2.393 + 6.228 * S - 0.881 * S2
       BD  =  46.06 + 4.673 * S - 14.98 * S2 + 1.331 * S3
       CD  =  17.83 - 53.47 * S + 21.24 * S2
       DD  =  4.081 + 0.976 * S - 0.485 * S2 + 0.152 * S3
       DV  = FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.070 + 0.042 * S - 0.011 * S2 + 0.004 * S3 
       AKE =  0.409 - 0.007 * S
       BKE =  0.782 + 0.082 * S
       AE  = -29.65 + 26.49 * S + 5.429 * S2
       BE  =  90.20 - 74.97 * S + 4.526 * S2 
       CE  =  0.0
       DE  =  8.122 + 2.120 * S - 1.088 * S2 + 0.231 * S3
       DEL = FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  0.877
       BEX =  0.561
       AKX =  0.275 
       BKX =  0.0
       AGX =  0.997
       BGX =  3.210 - 1.866 * S
       CX  =  7.300
       DX  =  9.010 + 0.896 * DS + 0.222 * S2
       EX  =  3.077 + 1.446 * S
       ESX =  3.173 - 2.445 * DS + 2.207 * S
       UDB = FW (X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.756
       BES =  0.216
       AKS =  1.690 + 0.650 * DS - 0.922 * S
       AS  = -4.329 + 1.131 * S
       BS  =  9.568 - 1.744 * S
       DST =  9.377 + 1.088 * DS - 1.320 * S + 0.130 * S2
       EST =  3.031 + 1.639 * S
       ESS =  5.837 + 0.815 * S
       SB  = FWS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  1.014
       BEG =  1.738
       AKG =  1.724 + 0.157 * S
       BKG =  0.800 + 1.016 * S
       AG  =  7.517 - 2.547 * S
       BG  =  34.09 - 52.21 * DS + 17.47 * S
       CG  =  4.039 + 1.491 * S
       DG  =  3.404 + 0.830 * S
       EG  = -1.112 + 3.438 * S  - 0.302 * S2
       ESG =  3.256 - 0.436 * S 
       GL  = FW (X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
