* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                   *
*     G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S      *
*                                                                   *
*                          1998 UPDATE                              *
*                                                                   *
*                  For a detailed explanation see                   *
*                   M. Glueck, E. Reya, A. Vogt :                   *
*        hep-ph/9806404  =  DO-TH 98/07  =  WUE-ITP-98-019          *
*                  (To appear in Eur. Phys. J. C)                   *
*                                                                   *
*   This package contains subroutines returning the light-parton    *
*   distributions in NLO (for the MSbar and DIS schemes) and LO;    * 
*   the respective light-parton, charm, and bottom contributions    *
*   to F2(electromagnetic); and the scale dependence of alpha_s.    *
*                                                                   *
*   The parton densities and F2 values are calculated from inter-   *
*   polation grids covering the regions                             *
*         Q^2/GeV^2  between   0.8   and  1.E6 ( 1.E4 for F2 )      *
*            x       between  1.E-9  and   1.                       *
*   Any call outside these regions stops the program execution.     *
*                                                                   *
*   At Q^2 = MZ^2, alpha_s reads  0.114 (0.125) in NLO (LO); the    *
*   heavy quark thresholds, QH^2 = mh^2, in the beta function are   *
*            mc = 1.4 GeV,  mb = 4.5 GeV,  mt = 175 GeV.            *
*   Note that the NLO alpha_s running is different from GRV(94).    * 
*                                                                   *
*    Questions, comments etc to:  avogt@physik.uni-wuerzburg.de     *
*                                                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*
*
*
      SUBROUTINE GRV98PA (ISET, X, Q2, UV, DV, US, DS, SS, GL)
*********************************************************************
*                                                                   *
*   THE PARTON ROUTINE.                                             *
*                                     __                            *
*   INPUT:   ISET =  1 (LO),  2 (NLO, MS), or  3 (NLO, DIS)         *
*            X  =  Bjorken-x        (between  1.E-9 and 1.)         *
*            Q2 =  scale in GeV**2  (between  0.8 and 1.E6)         *
*                                                                   *
*   OUTPUT:  UV = u - u(bar),  DV = d - d(bar),  US = u(bar),       *
*            DS = d(bar),  SS = s = s(bar),  GL = gluon.            *
*            Always x times the distribution is returned.           *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINIP / IINIP , and the     *
*            integer variable  IINIP  has always to be zero when    *
*            GRV98PA is called for the first time or when  ISET     *
*            has been changed.                                      *
*                                                                   *
*   GRIDS:   1. grv98lo.grid, 2. grv98nlm.grid, 3. grv98nld.grid,   *
*            (1+1809 lines with 6 columns, 4 significant figures)   *
*                                                                   *
*******************************************************i*************
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (NPART=6, NX=68, NQ=27, NARG=2)
      DIMENSION XUVF(NX,NQ), XDVF(NX,NQ), XDEF(NX,NQ), XUDF(NX,NQ),
     1          XSF(NX,NQ), XGF(NX,NQ), PARTON (NPART,NQ,NX-1), 
     2          QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ) 
      CHARACTER*80 LINE
      COMMON / INTINIP / IINIP
      SAVE XUVF, XDVF, XDEF, XUDF, XSF, XGF, NA, ARRF
*
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8E0, 
     1           1.0E0, 1.3E0, 1.8E0, 2.7E0, 4.0E0, 6.4E0,
     2           1.0E1, 1.6E1, 2.5E1, 4.0E1, 6.4E1,
     3           1.0E2, 1.8E2, 3.2E2, 5.7E2,
     4           1.0E3, 1.8E3, 3.2E3, 5.7E3,
     5           1.0E4, 2.2E4, 4.6E4,
     6           1.0E5, 2.2E5, 4.6E5, 
     7           1.E6 /
       DATA XB / 1.0E-9, 1.8E-9, 3.2E-9, 5.7E-9, 
     A           1.0E-8, 1.8E-8, 3.2E-8, 5.7E-8, 
     B           1.0E-7, 1.8E-7, 3.2E-7, 5.7E-7, 
     C           1.0E-6, 1.4E-6, 2.0E-6, 3.0E-6, 4.5E-6, 6.7E-6,
     1           1.0E-5, 1.4E-5, 2.0E-5, 3.0E-5, 4.5E-5, 6.7E-5,
     2           1.0E-4, 1.4E-4, 2.0E-4, 3.0E-4, 4.5E-4, 6.7E-4,
     3           1.0E-3, 1.4E-3, 2.0E-3, 3.0E-3, 4.5E-3, 6.7E-3,
     4           1.0E-2, 1.4E-2, 2.0E-2, 3.0E-2, 4.5E-2, 0.06, 0.08,
     5           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     6           0.3, 0.325, 0.35, 0.375, 0.4,  0.45, 0.5, 0.55,
     7           0.6, 0.65,  0.7,  0.75,  0.8,  0.85, 0.9, 0.95, 1. /
*
*...CHECK OF X AND Q2 VALUES : 
      IF ( (X.LT.0.99D-9) .OR. (X.GT.1.D0) ) THEN
         WRITE(6,91) 
  91     FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
         STOP
      ENDIF
      IF ( (Q2.LT.0.799) .OR. (Q2.GT.1.01E6) ) THEN
         WRITE(6,92) 
  92     FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
         STOP
      ENDIF
      IF (IINIP .NE. 0) GOTO 16
*
*...INITIALIZATION, IF REQUIRED :
*
*    SELECTION AND READING OF THE GRID : 
*    (COMMENT: FIRST NUMBER IN THE FIRST LINE OF THE GRID)
      IF (ISET .EQ. 1) THEN
        OPEN (11,FILE='grids/grv98lo.grid',STATUS='old')   ! 7.332E-05
      ELSE IF (ISET .EQ. 2) THEN
        OPEN (11,FILE='grids/grv98nlm.grid',STATUS='old')  ! 1.015E-04
      ELSE IF (ISET .EQ. 3) THEN
        OPEN (11,FILE='grids/grv98nld.grid',STATUS='old')  ! 1.238E-04
      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'NO OR INVALID PARTON SET CHOICE')
        STOP
      END IF
      IINIP = 1
      READ(11,89) LINE
  89  FORMAT(A80)
      DO 15 M = 1, NX-1 
      DO 15 N = 1, NQ
      READ(11,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1            PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M) 
  90  FORMAT (6(1PE10.3))
  15  CONTINUE
      CLOSE(11)
*
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0V = XB(IX)**0.5 
        XB0S = XB(IX)**(-0.2) 
        XB1 = 1.-XB(IX)
        XUVF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0V)
        XDVF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0V)
        XDEF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**7 * XB0V) 
        XUDF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**7 * XB0S)
        XSF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**7 * XB0S)
        XGF(IX,IQ)  = PARTON(6,IQ,IX) / (XB1**5 * XB0S)
  20  CONTINUE
        XUVF(NX,IQ) = 0.E0
        XDVF(NX,IQ) = 0.E0
        XDEF(NX,IQ) = 0.E0
        XUDF(NX,IQ) = 0.E0
        XSF(NX,IQ)  = 0.E0
        XGF(NX,IQ)  = 0.E0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
*
*...CONTINUATION, IF INITIALIZATION WAS DONE PREVIOUSLY.
*
  16  CONTINUE
*
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      X1 = 1.- X
      XV = X**0.5
      XS = X**(-0.2)
      UV = FINT(NARG,XT,NA,ARRF,XUVF) * X1**3 * XV
      DV = FINT(NARG,XT,NA,ARRF,XDVF) * X1**4 * XV
      DE = FINT(NARG,XT,NA,ARRF,XDEF) * X1**7 * XV
      UD = FINT(NARG,XT,NA,ARRF,XUDF) * X1**7 * XS
      US = 0.5 * (UD - DE)
      DS = 0.5 * (UD + DE)
      SS = FINT(NARG,XT,NA,ARRF,XSF)  * X1**7 * XS
      GL = FINT(NARG,XT,NA,ARRF,XGF)  * X1**5 * XS 
*
 60   RETURN
      END
*
*
*
*
      SUBROUTINE GRV98F2 (ISET, X, Q2, F2L, F2C, F2B, F2)
*********************************************************************
*                                                                   *
*   THE F2 ROUTINE.                                                 *
*                                                                   *
*   INPUT :  ISET = 1 (LO),  2 (NLO).                               *
*            X  =  Bjorken-x        (between  1.E-9 and 1.)         *
*            Q2 =  scale in GeV**2  (between  0.8 and 1.E4)         *
*                                                                   *
*   OUTPUT:  F2L = F2(light), F2C = F2(charm), F2B = F2(bottom,)    *
*            F2  = sum, all given for electromagnetic proton DIS.   *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINIF / IINIF , and the     *
*            integer variable  IINIF  has always to be zero when    *
*            GRV98F2 is called for the first time or when  ISET     *
*            has been changed.                                      *
*                                                                   *
*   GRIDS:   1. grv98lof.grid, 2. grv98nlf.grid.                    *
*            (1+1407 lines with 3 columns, 4 significant figures)   *
*                                                                   *
*******************************************************i*************
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (NSTRF=3, NX=68, NQ=21, NARG=2)
      DIMENSION XF2LF(NX,NQ), XF2CF(NX,NQ), XF2BF(NX,NQ), 
     1          STRFCT (NSTRF,NQ,NX-1), QS(NQ), XB(NX), 
     3          XT(NARG), NA(NARG), ARRF(NX+NQ) 
      CHARACTER*80 LINE
      COMMON / INTINIF / IINIF
      SAVE XF2LF, XF2CF, XF2BF, NA, ARRF
*
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8E0, 
     1           1.0E0, 1.3E0, 1.8E0, 2.7E0, 4.0E0, 6.4E0,
     2           1.0E1, 1.6E1, 2.5E1, 4.0E1, 6.4E1,
     3           1.0E2, 1.8E2, 3.2E2, 5.7E2,
     4           1.0E3, 1.8E3, 3.2E3, 5.7E3,
     5           1.0E4/ 
       DATA XB / 1.0E-9, 1.8E-9, 3.2E-9, 5.7E-9, 
     A           1.0E-8, 1.8E-8, 3.2E-8, 5.7E-8, 
     B           1.0E-7, 1.8E-7, 3.2E-7, 5.7E-7, 
     C           1.0E-6, 1.4E-6, 2.0E-6, 3.0E-6, 4.5E-6, 6.7E-6,
     1           1.0E-5, 1.4E-5, 2.0E-5, 3.0E-5, 4.5E-5, 6.7E-5,
     2           1.0E-4, 1.4E-4, 2.0E-4, 3.0E-4, 4.5E-4, 6.7E-4,
     3           1.0E-3, 1.4E-3, 2.0E-3, 3.0E-3, 4.5E-3, 6.7E-3,
     4           1.0E-2, 1.4E-2, 2.0E-2, 3.0E-2, 4.5E-2, 0.06, 0.08,
     5           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     6           0.3, 0.325, 0.35, 0.375, 0.4,  0.45, 0.5, 0.55,
     7           0.6, 0.65,  0.7,  0.75,  0.8,  0.85, 0.9, 0.95, 1. /
*
*...CHECK OF X AND Q2 VALUES : 
      IF ( (X.LT.0.99D-9) .OR. (X.GT.1.D0) ) THEN
         WRITE(6,91) 
  91     FORMAT (2X,'STR.FCT. INTERPOLATION: X OUT OF RANGE')
         STOP
      ENDIF
      IF ( (Q2.LT.0.799) .OR. (Q2.GT.1.01E4) ) THEN
         WRITE(6,92) 
  92     FORMAT (2X,'STR.FCT. INTERPOLATION: Q2 OUT OF RANGE')
         STOP
      ENDIF
      IF (IINIF .NE. 0) GOTO 16
*
*...INITIALIZATION, IF REQUIRED :
*
*    SELECTION AND READING OF THE GRID : 
*    (COMMENT: FIRST NUMBER IN THE FIRST LINE OF THE GRID)
      IF (ISET .EQ. 1) THEN
        OPEN (11,FILE='grv98lof.grid',STATUS='old')  !  7.907E-01
      ELSE IF (ISET.EQ.2) THEN
        OPEN (11,FILE='grv98nlf.grid',STATUS='old')  !  9.368E-01
      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'NO OR INVALID STR.FCT. SET CHOICE')
        STOP
      END IF
      IINIF = 1
      READ(11,89) LINE
  89  FORMAT(A80)
      DO 15 M = 1, NX-1 
      DO 15 N = 1, NQ
      READ(11,90) STRFCT(1,N,M), STRFCT(2,N,M), STRFCT(3,N,M) 
  90  FORMAT (3(1PE10.3))
  15  CONTINUE
      CLOSE(11)
*
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XBS = XB(IX)**0.2 
        XB1 = 1.-XB(IX)
        XF2LF(IX,IQ) = STRFCT(1,IQ,IX) / XB1**3 * XBS
        XF2CF(IX,IQ) = STRFCT(2,IQ,IX) / XB1**7 * XBS
        XF2BF(IX,IQ) = STRFCT(3,IQ,IX) / XB1**7 * XBS 
  20  CONTINUE
        XF2LF(NX,IQ) = 0.E0
        XF2CF(NX,IQ) = 0.E0
        XF2BF(NX,IQ) = 0.E0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
*
*...CONTINUATION, IF INITIALIZATION WAS DONE PREVIOUSLY.
*
  16  CONTINUE
*
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      X1 = 1.- X
      XS = X**(-0.2)
      F2L = FINT(NARG,XT,NA,ARRF,XF2LF) * X1**3 * XS
      F2C = FINT(NARG,XT,NA,ARRF,XF2CF) * X1**7 * XS
      F2B = FINT(NARG,XT,NA,ARRF,XF2BF) * X1**7 * XS
      F2  = F2L + F2C + F2B
*
 60   RETURN
      END
*
*
*
*
      FUNCTION FINT(NARG,ARG,NENT,ENT,TABLE)
*********************************************************************
*                                                                   *
*   THE INTERPOLATION ROUTINE (CERN LIBRARY ROUTINE E104)           *
*                                                                   *
*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION ARG(5),NENT(5),ENT(10),TABLE(10)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      FINT=0.
   10 FAC=1.
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      FINT=FINT+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END
*
*
*
*
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
*
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


