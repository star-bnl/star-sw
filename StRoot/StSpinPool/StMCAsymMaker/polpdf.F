*********************************************************************
*                                                                   *
*        POLARIZED PARTON DISTRIBUTION FUNCTION WITH ERRORS         *
*                        FOR LO AND NLO                             *
*                                                                   *
*            Johannes Bluemlein and Helmut Boettcher                *
*                        hep-ph/0203155                             *
*                                                                   *
*   PROBLEMS/QUESTIONS TO blumlein@ifh.de OR hboett@ifh.de          *
*                                                                   *
*   INPUT:   iset = number of the parton set :                      *
*                                                                   *
*            iset = 1  NEXT-TO-LEADING ORDER (MS-bar)               *
*                      data file: hermes_xpdf_nlo_7p.grid           * 
*                                                                   *
*            x  = Bjorken-x       (between  1.0E-09 and 1.0E+0)     *
*            q2 = scale in GeV**2 (between  1.0E+00 and 1.0E+6)     *
*                                                                   *
*   OUTPUT:  UV = DELTA u_v    & error: DUV                         *
*            DV = DELTA d_v    & error: DDV                         *
*            GL = DELTA gluon  & error: DGL                         *
*            QB = DELTA qbar   & error: DQB                         *   
*            G1P = g_1^proton  & error: DG1P                        *
*            G1N = g_1^neutron & error: DG1N                        * 
*                                                                   *
*            NOTE:                                                  *
*            -----                                                  *
*            For the parton distributions and for g1(p,n)           *
*            always x times the distribution is returned.           *
*                                                                   *
*            The sets are the result of a combined fit to the       *
*            world data for the spin asymmetries, i.e. A_1(p,n,d)   *
*            or g_1/F_1(p,n,d).                                     *
*                                                                   *
*            The subroutine POLPDF returns the BB polarized parton  *
*            distribution function values and g1(p,n) at the given  *
*            point in Q**2 and XB by interpolating the data on the  *
*            specified grid.                                        *
*                                                                   *
*            Note: No charm is included                             *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be ZERO when POLPDF is called for the    *
*            first time or when 'ISET' has been changed.            *
*                                                                   *
*********************************************************************
*
      SUBROUTINE POLPDF(ISET, X, Q2, UV, DUV, DV, DDV, GL, DGL,
     1                QB, DQB, G1P, DG1P, G1N, DG1N)
*     ---------------------------------------------------------------
*
      IMPLICIT REAL*8 (A-H,O-Z)
*     
      PARAMETER (NPDF=6, NQ=51, NX=104 )
*
      CHARACTER*2  STAR
      CHARACTER*1024 FILENAME
*
      COMMON / INTINI / IINI
*
      DIMENSION QS(NQ),XB(NX),XPDF(NX,NQ,NPDF),XDPDF(NX,NQ,NPDF)
      DIMENSION AXB(NX,NQ,NPDF),BXB(NX,NQ,NPDF),CXB(NX,NQ,NPDF)
      DIMENSION AXBE(NX,NQ,NPDF),BXBE(NX,NQ,NPDF),CXBE(NX,NQ,NPDF)
*
      DIMENSION PDF1(NPDF),PDF2(NPDF),DPDF1(NPDF),DPDF2(NPDF)
      DIMENSION PDF(NPDF),DPDF(NPDF)
*
      SAVE QS, XB, XPDF, XDPDF
      SAVE AXB, BXB, CXB, AXBE, BXBE, CXBE
*
*     BJORKEN-X AND Q**2 VALUES OF THE GRID WHICH WILL BE READ IN 
*     TOGETHER WITH THE OTHER DATA :
*
c      DATA QS /
c     1 0.10000D+01, 0.13183D+01, 0.17378D+01, 0.22909D+01,0.30200D+01,
c     2 0.39811D+01, 0.52481D+01, 0.69183D+01, 0.91201D+01,0.12023D+02,
c     3 0.15849D+02, 0.20893D+02, 0.27542D+02, 0.36308D+02,0.47863D+02,
c     4 0.63096D+02, 0.83176D+02, 0.10965D+03, 0.14454D+03,0.19055D+03,
c     5 0.25119D+03, 0.33113D+03, 0.43652D+03, 0.57544D+03,0.75858D+03,
c     6 0.10000D+04, 0.13183D+04, 0.17378D+04, 0.22909D+04,0.30200D+04,
c     7 0.39811D+04, 0.52481D+04, 0.69183D+04, 0.91201D+04,0.12023D+05,
c     8 0.15849D+05, 0.20893D+05, 0.27542D+05, 0.36308D+05,0.47863D+05,
c     9 0.63096D+05, 0.83176D+05, 0.10965D+06, 0.14454D+06,0.19055D+06,
c     A 0.25119D+06, 0.33113D+06, 0.43652D+06, 0.57544D+06,0.75858D+06,
c     B 0.10000D+07 /
c       DATA XB /
c     1 0.10000D-08, 0.13183D-08, 0.17378D-08, 0.22909D-08, 0.30200D-08,
c     2 0.39811D-08, 0.52481D-08, 0.69183D-08, 0.91201D-08, 0.12023D-07,
c     3 0.15849D-07, 0.20893D-07, 0.27542D-07, 0.36308D-07, 0.47863D-07,
c     4 0.63096D-07, 0.83176D-07, 0.10965D-06, 0.14454D-06, 0.19055D-06,
c     5 0.25119D-06, 0.33113D-06, 0.43652D-06, 0.57544D-06, 0.75858D-06,
c     1 0.10000D-05, 0.13183D-05, 0.17378D-05, 0.22909D-05, 0.30200D-05,
c     2 0.39811D-05, 0.52481D-05, 0.69183D-05, 0.91201D-05, 0.12023D-04,
c     3 0.15849D-04, 0.20893D-04, 0.27542D-04, 0.36308D-04, 0.47863D-04,
c     4 0.63096D-04, 0.83176D-04, 0.10965D-03, 0.14454D-03, 0.19055D-03,
c     5 0.25119D-03, 0.33113D-03, 0.43652D-03, 0.57544D-03, 0.75858D-03,
c     1 0.10000D-02, 0.13183D-02, 0.17378D-02, 0.22909D-02, 0.30200D-02,
c     2 0.39811D-02, 0.52481D-02, 0.69183D-02, 0.91201D-02, 0.12023D-01,
c     3 0.15849D-01, 0.20893D-01, 0.27542D-01, 0.36308D-01, 0.47863D-01,
c     4 0.63096D-01, 0.83176D-01,
c     6 0.10000D+00, 0.12500D+00, 0.15000D+00, 0.17500D+00, 0.20000D+00,
c     7 0.22500D+00, 0.25000D+00, 0.27500D+00, 0.30000D+00, 0.32500D+00,
c     8 0.35000D+00, 0.37500D+00, 0.40000D+00, 0.42500D+00, 0.45000D+00,
c     9 0.47500D+00, 0.50000D+00, 0.52500D+00, 0.55000D+00, 0.57500D+00,
c     A 0.60000D+00, 0.62500D+00, 0.65000D+00, 0.67500D+00, 0.70000D+00,
c     B 0.72500D+00, 0.75000D+00, 0.77500D+00, 0.80000D+00, 0.82500D+00,
c     C 0.85000D+00, 0.87500D+00, 0.90000D+00, 0.92500D+00, 0.95000D+00,
c     D 0.97500D+00, 0.10000D+01 /
*
*     CHECK OF X AND Q2 VALUES : 
*
      IF ( X.LT.1.0D-09 .OR. X.GT.1.0D+00 ) THEN
         WRITE(6,2000) 
 2000    FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE -- STOP')
         STOP
      ENDIF
      IF ( Q2.LT.1.0D+00 .OR. Q2.GT.1.0D+06 ) THEN
         WRITE(6,2001) 
 2001    FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE -- STOP')
         STOP
      ENDIF
*
      IF (IINI .NE. 0) GOTO 10
*
*     READING OF THE DATA SET :
*     
      LIN = 10
*
      IF (ISET .EQ. 1) THEN
*
*     open input unit for NLO data
*
         CALL GETENV('STAR',FILENAME)
         FILENAME = FILENAME(1:LEN_TRIM(FILENAME))
     1        //'/StRoot/StSpinPool/StMCAsymMaker/qcd_nlo_907_0.grid'
         OPEN(UNIT=LIN,FILE=FILENAME(1:LEN_TRIM(FILENAME)),
     1        STATUS='OLD')
*
      ELSE
*
         WRITE(6,2003)
 2003    FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE -- '
     1        //'STOP')
         STOP
*
      ENDIF
*
      READ(LIN,2004) STAR
 2004 FORMAT (A2)
      DO IQ = 1,NQ
         DO IX = 1,NX
            READ(LIN,2005) QS(IQ),XB(IX),
     1           (XPDF(IX,IQ,IPDF),XDPDF(IX,IQ,IPDF),IPDF=1,NPDF)
 2005       FORMAT (14(1P,E13.5))
         ENDDO
      ENDDO
*     
      CLOSE(LIN)
*
*     CALCULATE SPLINE COEFFIFIENTS FOR THE INTERPOLATION IN X :
*
      DO IPDF = 1,NPDF
         DO IQ = 1,NQ
            CALL SPLINE2(NX,XB,XPDF,AXB,BXB,CXB,IPDF,IQ)
            CALL SPLINE2(NX,XB,XDPDF,AXBE,BXBE,CXBE,IPDF,IQ)
         ENDDO
      ENDDO
*
      IINI = 1
*
 10   CONTINUE
*
*     INTERPOLATION :
*     X: CUBIC SPLINE INTERPOLATION, LOG(Q2): LINEAR INTERPOLATION
*
      IQ = ISEARCH(NQ,QS,Q2)
      IF (IQ .EQ. NQ) IQ = NQ-1
      IX = ISEARCH(NX,XB,X)
*
      DX = X - XB(IX)
*
      DO IPDF = 1,NPDF
         PDF1(IPDF) = XPDF(IX,IQ,IPDF)
     1        + DX*(AXB(IX,IQ,IPDF) + DX*(BXB(IX,IQ,IPDF) 
     2        + DX*CXB(IX,IQ,IPDF)))
         PDF2(IPDF) = XPDF(IX,IQ+1,IPDF)
     1        + DX*(AXB(IX,IQ+1,IPDF) + DX*(BXB(IX,IQ+1,IPDF) 
     2        + DX*CXB(IX,IQ+1,IPDF)))
         DPDF1(IPDF) = XDPDF(IX,IQ,IPDF)
     1        + DX*(AXBE(IX,IQ,IPDF) + DX*(BXBE(IX,IQ,IPDF) 
     2        + DX*CXBE(IX,IQ,IPDF)))
         DPDF2(IPDF) = XDPDF(IX,IQ+1,IPDF)
     1        + DX*(AXBE(IX,IQ+1,IPDF) + DX*(BXBE(IX,IQ+1,IPDF) 
     2        + DX*CXBE(IX,IQ+1,IPDF)))
      ENDDO
*
      TQ = (DLOG(Q2)-DLOG(QS(IQ))) / (DLOG(QS(IQ+1))-DLOG(QS(IQ)))
*
      DO IPDF = 1,NPDF
         PDF(IPDF)  = (1.0D0-TQ)*PDF1(IPDF) + TQ*PDF2(IPDF)
         DPDF(IPDF) = (1.0D0-TQ)*DPDF1(IPDF) + TQ*DPDF2(IPDF)
      ENDDO
*
      UV    = PDF(1)
      DUV   = DPDF(1)
      DV    = PDF(2)
      DDV   = DPDF(2)
      GL    = PDF(3)
      DGL   = DPDF(3)
      QB    = PDF(4)
      DQB   = DPDF(4)
      G1P   = PDF(5)
      DG1P  = DPDF(5)
      G1N   = PDF(6)
      DG1N  = DPDF(6)
*
      RETURN
      END
*
* ---------------------------------------------------------------------
      SUBROUTINE SPLINE2(N,X,Y,B,C,D,I,J)
* ---------------------------------------------------------------------
* CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
* INTERPOLATION SUBROUTINES ARE TAKEN FROM
* G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
* COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
*
* SUBROUTINE TAKEN FROM AAC GROUP (KUMANO et al.)
*
      IMPLICIT REAL*8(A-H,O-Z)
*
      PARAMETER (NPDF=6, NQ=51, NX=104 )
*
      DIMENSION X(NX), Y(NX,NQ,NPDF),
     1          B(NX,NQ,NPDF), C(NX,NQ,NPDF), D(NX,NQ,NPDF)
*
      NM1=N-1
      IF(N.LT.2) RETURN
      IF(N.LT.3) GO TO 250
      D(1,J,I)=X(2)-X(1)
      C(2,J,I)=(Y(2,J,I)-Y(1,J,I))/D(1,J,I)
      DO 210 K=2,NM1
         D(K,J,I)=X(K+1)-X(K)
         B(K,J,I)=2.0D0*(D(K-1,J,I)+D(K,J,I))
         C(K+1,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
         C(K,J,I)=C(K+1,J,I)-C(K,J,I)
  210 CONTINUE
      B(1,J,I)=-D(1,J,I)
      B(N,J,I)=-D(N-1,J,I)
      C(1,J,I)=0.0D0
      C(N,J,I)=0.0D0
      IF(N.EQ.3) GO TO 215
      C(1,J,I)=C(3,J,I)/(X(4)-X(2))-C(2,J,I)/(X(3)-X(1))
      C(N,J,I)=C(N-1,J,I)/(X(N)-X(N-2))-C(N-2,J,I)/(X(N-1)-X(N-3))
      C(1,J,I)=C(1,J,I)*D(1,J,I)**2.0D0/(X(4)-X(1))
      C(N,J,I)=-C(N,J,I)*D(N-1,J,I)**2.0D0/(X(N)-X(N-3))
 215  CONTINUE
      DO 220 K=2,N
         T=D(K-1,J,I)/B(K-1,J,I)
         B(K,J,I)=B(K,J,I)-T*D(K-1,J,I)
         C(K,J,I)=C(K,J,I)-T*C(K-1,J,I)
 220  CONTINUE
      C(N,J,I)=C(N,J,I)/B(N,J,I)
      DO 230 IB=1,NM1
         K=N-IB
         C(K,J,I)=(C(K,J,I)-D(K,J,I)*C(K+1,J,I))/B(K,J,I)
 230  CONTINUE
      B(N,J,I)=(Y(N,J,I)-Y(NM1,J,I))/D(NM1,J,I)
     1     +D(NM1,J,I)*(C(NM1,J,I)+2.0D0*C(N,J,I))
      DO 240 K=1,NM1
         B(K,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
     1        -D(K,J,I)*(C(K+1,J,I)+2.0D0*C(K,J,I))
         D(K,J,I)=(C(K+1,J,I)-C(K,J,I))/D(K,J,I)
         C(K,J,I)=3.0D0*C(K,J,I)
 240  CONTINUE
      C(N,J,I)=3.0D0*C(N,J,I)
      D(N,J,I)=D(N-1,J,I)
      RETURN
 250  CONTINUE
      B(1,J,I)=(Y(2,J,I)-Y(1,J,I))/(X(2)-X(1))
      C(1,J,I)=0.0D0
      D(1,J,I)=0.0D0
      B(2,J,I)=B(1,J,I)
      C(2,J,I)=0.0D0
      D(2,J,I)=0.0D0
      RETURN
      END
