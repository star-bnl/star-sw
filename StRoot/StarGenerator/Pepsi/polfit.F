
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            C
C       POLARIZED PARTON DISTRIBUTIONS (NLO and LO)                          C
C       FROM CERN-TH/71-97 AND HEP-PH 9711440                                C
C       D. de Florian, R. Sassot and O. Sampayo                              C
C                                                                            C
C       MODE: 1 SET1 NLO (MSbar)                                             C
C             2 SET2 NLO (MSbar)                                             C
C             3 SET3 NLO (MSbar)                                             C
C             4 SET1 LO                                                      C
C             5 SET2 LO                                                      C
C             6 SET3 LO                                                      C
C                                                                            C
C            Q2=Q^2                                                          C
C            DUV :    X * U VALENCE DISTRIBUTION                             C
C            DDV :    X * D VALENCE DISTRIBUTION                             C
C            DQBAR :  X * UBAR= X * DBAR DISTRIBUTION                        C
C            DSTR :   X * STR= X * STRBAR DISTRIBUTION                       C
C            DGLU :   X * GLUON DISTRIBUTION                                 C
C            G1P :    X * POLARIZED STRUCTURE FUNCTION (PROTON)              C
C            G1N :    X * POLARIZED STRUCTURE FUNCTION (NEUTRON)             C
C            G1D :    X * POLARIZED STRUCTURE FUNCTION(DEUTERON)             C
C                                                                            C
C       REMEMBER: ALWAYS X*DISTRIBUTION !!!                                  C
C        BEFORE CALLING THE SUBRUTINE `POLFIT` FOR THE FIRST TIME, THE       C
C       SUBROUTINE `INI` MUST BE CALLED (ONLY ONCE) TO READ THE GRIDS.       C
C              (CALL INI)                                                    C
C                                                                            C
C       RANGE OF VALIDITY OF THE INTERPOLATION:                              C
C                                   10**(-4)< X < 0.9                        C
C                                   1 < Q**2 < 5*10**4                       C
C                                                                            C
C       IN CASE OF PROBLEMS, DOUBTS, ETC, PLEASE REPORT TO                   C
C        Daniel.de.Florian@cern.ch                                           C
C                                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE POLFIT(MODE,X,Q2,DUV,DDV,DQBAR,DSTR,DGLU)
************************      
*HI    Speed optimized *
************************      
       IMPLICIT NONE

      COMMON/ XARRAY / XARRAY
      COMMON/ GRID / GU,GD,GL,GS,GG,GP,GN
      DOUBLE PRECISION XARRAY(102), GU(76,26), GD(76,26), GL(76,26),
     &     GS(76,26), GG(76,26), GP(76,26), GN(76,26)

      DOUBLE PRECISION XQ(2),PERINOLA,X3,X4,X5,X6,X7,X,Q2
      DOUBLE PRECISION DUV,DDV,DQBAR,DSTR,DGLU
      INTEGER MODE

      XQ(1) = X
      XQ(2) = Q2
      X3=(1.D0-XQ(1))**3.D0
      X4=(1.D0-XQ(1))**4.D0
      X5=XQ(1)**0.5D0
      X6=XQ(1)**0.6D0
      X7=XQ(1)**0.7D0
      DUV = PERINOLA(XQ,GU) * X3* X6
      DDV = PERINOLA(XQ,GD) * X4 * X7 
      DQBAR = PERINOLA(XQ,GL) * X3 * X5
      DSTR = PERINOLA(XQ,GS)  * X3 * X5
      DGLU = PERINOLA(XQ,GG)  * X3 * X5
*HI      G1P = PERINOLA(XQ,GP)  * X3 * X5
*HI      G1N = PERINOLA(XQ,GN)  * X4 * X5
*HI      G1D=(G1P+G1N)*0.5D0*(1.D0-1.5D0*0.058D0)
      RETURN
      END
