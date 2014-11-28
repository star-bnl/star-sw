*
*********************************************************************
*                                                                   *
*    POLARIZED RADIATIVELY GENERATED LO AND NLO PARTON DENSITIES    *
*                                                                   *
*         M. GLUCK, E. REYA, M. STRATMANN AND W. VOGELSANG,         *
*                        hep-ph/0011215                             *
*                                                                   *
*          PROBLEMS/QUESTIONS TO wvogelsang@bnl.gov                 *
*            OR TO marco.stratmann@physik.uni-regensburg.de         *
*                                                                   *
*   INPUT:   ISET = number of the parton set :                      *
*            ISET = 1  'STANDARD' SCENARIO, NEXT-TO-LEADING ORDER   *
*                      (MS-bar)                                     * 
*                      (DATA FILE 'std2000_nlo.grid' UNIT=11, TO BE *
*                       DEFINED BY THE USER )                       *
*            ISET = 2  'VALENCE' SCENARIO,  NEXT-TO-LEADING ORDER   *
*                      (MS-bar)                                     *   
*                      (DATA FILE 'val2000_nlo.grid' UNIT=22, TO BE *
*                       DEFINED BY THE USER )                       *
*            ISET = 3  'STANDARD' SCENARIO, LEADING ORDER           *
*                      (DATA FILE 'std2000_lo.grid' UNIT=33, TO BE  *
*                       DEFINED BY THE USER )                       *
*            ISET = 4  'VALENCE' SCENARIO,  LEADING ORDER           *
*                      (DATA FILE 'val2000_lo.grid' UNIT=44, TO BE  *
*                       DEFINED BY THE USER )                       *
*                                                                   *
*            X  = Bjorken-x       (between  1.E-4  and  1)          *
*            Q2 = scale in GeV**2 (between  0.8  and   1.E6)        *
*                                                                   *
*   OUTPUT:  U = x * DELTA u                                        *
*            D = x * DELTA d                                        *        
*            UB = x * DELTA ubar                                    *   
*            DB = x * DELTA dbar                                    * 
*            ST = x * DELTA STRANGE                                 *     
*            GL = x * DELTA GLUON                                   *
*            G1P = g_1^proton                                       *
*            G1N = g_1^neutron                                      * 
*                                                                   *
*          (  For the parton distributions always x times           *
*                   the distribution is returned .                  *
*                 This is NOT the case for g1(p,n)  )               *
*                                                                   *
*            The sets are the result of a combined fit to           *
*            data for the spin asymmetries A_1 (p,n,d)              *
*                                                                   *
*            Note: No charm is included                             *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be zero when PARPOL is called for the    *
*            first time or when 'ISET' has been changed.            *
*                                                                   *
*********************************************************************
*
      SUBROUTINE PARPOLNEW (ISET, X, Q2, U, D, UB, DB, ST,GL,G1P,G1N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NPART=8, NX=42, NQ=30, NARG=2)
      DIMENSION XUF(NX,NQ), XDF(NX,NQ), XUBF(NX,NQ), XDBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ), XG1P(NX,NQ), XG1N(NX,NQ),
     2          PARTON (NPART,NQ,NX-1), QS(NQ), XB(NX), XT(NARG), 
     3          NA(NARG), ARRF(NX+NQ) 
      COMMON / INTINI / IINI
      SAVE XUF, XDF, XUBF, XDBF, XSF, XGF, XG1P, XG1N, NA, ARRF
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8D0, 1.0D0, 1.25d0, 1.5D0, 2.d0, 2.5D0, 
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4, 1.8D4, 3.2D4, 5.8D4, 
     4           1.0D5, 1.8D5, 3.2D5, 5.8D5, 1.0D6  /
       DATA XB / 
     1           1.D-4, 1.5D-4, 2.2D-4, 3.2D-4, 4.8D-4, 7.D-4,
     2           1.D-3, 1.5D-3, 2.2D-3, 3.2D-3, 4.8D-3, 7.D-3,
     3           1.D-2, 1.5D-2, 2.2D-2, 3.2D-2, 5.0D-2, 7.5D-2,
     4           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     5           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     6           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9, 1.0 /

#include "pepadm.inc"

*...CHECK OF X AND Q2 VALUES : 
       IF ( (X.LT.1.0D-6) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91) 
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
           STOP
c          GOTO 60
       ENDIF
       IF ( (Q2.LT.0.8D0) .OR. (Q2.GT.1.D6) ) THEN
           WRITE(6,92) 
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
           STOP
C          GOTO 60
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
*    FILE - NO. = 11 FOR NLO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.3478E-03 )
*    FILE - NO. = 22 FOR NLO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.5146E-05 )
*    FILE - NO. = 33 FOR  LO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 3.4686E-03 )     
*    FILE - NO. = 44 FOR  LO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 2.4395E-04 )
      IF (IINI.NE.0) GOTO 16
      IF (ISET.EQ.1) THEN
       IIREAD=IPLST(3)       
       OPEN(UNIT=IPLST(3),FILE=CPOL,STATUS='OLD')
      ELSE IF (ISET.EQ.2) THEN
       IIREAD=IPLST(3)
       OPEN(UNIT=IPLST(3),FILE=CPOL,STATUS='OLD')
      ELSE IF (ISET.EQ.3) THEN
       IIREAD=IPLST(3)       
       OPEN(UNIT=IPLST(3),FILE=CPOL,STATUS='OLD')
      ELSE IF (ISET.EQ.4) THEN
       IIREAD=IPLST(3)
       OPEN(UNIT=IPLST(3),FILE=CPOL,STATUS='OLD')
      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE')
        GOTO 60
      END IF
C
       DO 15 M = 1, NX-1
       DO 15 N = 1, NQ
       READ(IIREAD,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M),
     2                 PARTON(7,N,M), PARTON(8,N,M)
  90   FORMAT (8(1PE12.4))
  15   CONTINUE
	close(iiread)
C
      IINI = 1
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0 = XB(IX) 
        XB1 = 1.D0-XB(IX)
        XUF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0)
        XDF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0)
        XUBF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**8 * XB0**0.5) 
        XDBF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**8 * XB0**0.5) 
        XSF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**8 * XB0**0.5) 
        XGF(IX,IQ)  = PARTON(6,IQ,IX) / (XB1**5 * XB0**2.)
        XG1P(IX,IQ)  = PARTON(7,IQ,IX) / XB1**3
        XG1N(IX,IQ)  = PARTON(8,IQ,IX) / XB1**3
  20  CONTINUE
        XUF(NX,IQ) = 0.D0
        XDF(NX,IQ) = 0.D0
        XUBF(NX,IQ) = 0.D0
        XDBF(NX,IQ) = 0.D0
        XSF(NX,IQ)  = 0.D0
        XGF(NX,IQ)  = 0.D0
        XG1P(NX,IQ)  = 0.D0
        XG1N(NX,IQ)  = 0.D0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      U = DFINTNEW(NARG,XT,NA,ARRF,XUF) * (1.D0-X)**3 * X
      D = DFINTNEW(NARG,XT,NA,ARRF,XDF) * (1.D0-X)**4 * X
      UB = DFINTNEW(NARG,XT,NA,ARRF,XUBF) * (1.D0-X)**8 * X**0.5
      DB = DFINTNEW(NARG,XT,NA,ARRF,XDBF) * (1.D0-X)**8 * X**0.5
      ST = DFINTNEW(NARG,XT,NA,ARRF,XSF)  * (1.D0-X)**8 * X**0.5
      GL = DFINTNEW(NARG,XT,NA,ARRF,XGF)  * (1.D0-X)**5 * X**2.
      G1P = DFINTNEW(NARG,XT,NA,ARRF,XG1P)  * (1.D0-X)**3
      G1N = DFINTNEW(NARG,XT,NA,ARRF,XG1N)  * (1.D0-X)**3
 60   RETURN
      END
