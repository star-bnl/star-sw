*DECK,CFIT. ==========================================================
      SUBROUTINE CFIT (Good, Goodsc, Mode, Isw, G, N1g, C, N1c, Ng, Nc, 
     & Xf, Nx, Gf, N1gf, Xi2, W, USER, Ierr)
************************************************************************
* CFIT                                                                 *
*                                                                      *
*  Created     :  8-JUN-1994   Author : Victor Perevoztchikov          *
*                                                                      *
*  Function    :  Fit with constrains, user routine called for         *
*                 calculation constrains residuals and gradients       *
*                                                                      *
*  References  : None                                                  *
*                                                                      *
*  Arguments   :                                                       *
*    Good      Convergence criterion (residual/error)**2               *
*    Goodsc    Scale factor for convergence for each constraint        *
*                                                                      *
*    Mode      init + 10*Niter                                         *
*              where                                                   *
*              Init      0= G correlation matrix is not used,          *
*			    info from the previous call is used        *
*			    Refit, only Isw and Xf is used             *
*			 2= G is covariant (error) matrix              *
*                        1= G is weight matrix (inverse of error mtx)  *
*              Niter     maximal number of iteration allowed           *
*    Isw       switch array, Isw(1:Nx)=0       fixed variable          *
*                            Isw(Nx+1:Nx+Nc)=0 constrain ignored       *
*    G         error mtx of measured values                            *
*    N1g       first dimension of G                                    *
*    C         mtx contains values of constrains & gradients in Xb+X0  *
*              C(1,N) 	== residual of Nth constrain                   *
*	       C(1+I,N) == dC(N)/dX(I)                                 *
*    N1c       first dimension of C                                    *
*    Ng        number of measured values, Ng <= N1g                    *
*    Nc        number of constrains, Nc+1 <= N1c                       *
*    Xf        fited corrections X=Xb+Xf                               *
*    Nx        size of X, Nx >= Ng                                     *
*    Xi2       Chisquare of fit                                        *
*    Gf        error matrix of fitted errors                           *
*    N1gf      first dimension of GF                                   *
*    W         work array of size (Nx+Nc)*(Nx+Nc+3)+Ng*Ng              *
*    Ierr      error flag                                              *
*    USER      User routine to calculate C                             *
*              Call USER (Itest, Xf, Nx, C, N1c, Nc, Ifail)            *
*              Where:                                                  *
*              Itest - mode of work                                    *
*                      0=normal                                        *
*                      1=test mode, no modify X                        *
*              Xf - vector of parameters (Input/Output)                *
*              C - MTX of constrains     (Ouput)                       *
*              Ifail   	0 = OK                                         *
*                       1 = too big step, cut step is needed           *
*              		9 = stop the fit                               *
*                                                                      *
*  Errors      :                                                       *
*    0 = Ok                                                            *
*    1 = bad error mtx (G)                                             *
*    2 = No solution                                                   *
*    3 = Too many iters                                                *
*    4 = Too many cuts                                                 *
*    9 = Extra big Chisq                                               *
*    10+User stop                                                      *
*                                                                      *
*  	Extra output in COMMON/SLATE/ISLAT(40)                         *
*       ISLAT(1) = number of iterations were done                      *
*                                                                      *
************************************************************************
*                                                                      *
* Formal parameters                                                    *
*____________________                                                  *
      REAL G(N1g,Ng),Xf(Nx),Gf(N1gf,Nx)
      REAL Good, Goodsc(*)

      INTEGER   Isw(Nx+Nc)
      PARAMETER (NDBL=2)
      REAL *8 W(*),DMBIL,C(N1c,Nc)

*
*
* Global sections
*__________________
      COMMON /SLATE/ ISLAT(40)
      EXTERNAL       USER
*
*
* Local variables
*__________________
      LOGICAL        XXcut
***      DATA           Mxcut/9/,Good/0.0001/,DXi2/900./,NNtest/1/
***      DATA           Mxcut/9/,Good/0.01/,DXi2/900./,NNtest/1/
***      DATA           Mxcut/9/,Good/0.1/,DXi2/900./,NNtest/1000/
      DATA           Mxcut/9/,DXi2/900./,NNtest/1/
*
*
* Executable statements
*________________________
*

      Nmx = Nx + Nc
      Ncc = 0
      Ngg = 0
      Nxx = 0
      DO j=1,Nmx
        IF (Isw(j).NE.0) THEN
          IF (j.LE.Ng) Ngg = Ngg + 1
          IF (j.LE.Nx) Nxx = Nxx + 1
          IF (j.GT.Nx) Ncc = Ncc + 1
        ENDIF
      ENDDO
*
      Ndf = Ncc - Nxx + Ngg
      Xi2max = DXi2*Ndf
      Lx0 = 0
      Lxf = Lx0 + Nmx
      Lmx = Lxf + Nmx
      Lcv = LMX + Nmx**2
*
      Lcv11 = Lcv+1
      Lcv21 = Lcv11+1
      Lcv12 = Lcv11+Ng
*
      IT    = 0
      Cqua  = 1.E+10
      Xqua  = 1.E+10
      CSum  = 1.E+10
       
      Init  = MOD(Mode,10)
      Niter = Mode/10
      Ncut  = 0
      Mo = Init 
      Istop = 0
      CALL VZERO (W(Lxf+Nx+1), Nc*NDBL)
      DO J=1,Nx
        W(Lxf+J) = Xf(J) 
      ENDDO
      CALL USER (0, Isw, W(LXf+1), Nx, C(1,1), N1c, Nc, Istop)
      Ierr   = 10 + Istop
      IF (Istop.NE.0)           GO TO 999
      
*
**          Test Derivatives
      IF (NNtest.GT.0) THEN
        NNtest = NNtest - 1
        CALL CFITST (USER, Isw, W(Lxf+1), Nx, C, N1c, Nc, ITerr)
      ENDIF
      Xi2 = 1.E+10
*
**              Start of loop
  100 Xquaqq = Xqua
  101 IT    = IT + 1
      Ierr  = 3
      IF (IT.GT.Niter)          GO TO 999
*
      CALL UCOPY(W(Lxf+1), W(Lx0+1), Nmx*NDBL)
*
  102 Xi2qq = Xi2
      Cquaqq = Cqua
      CSumqq = CSum
*
      CALL CFITGO (Mo, Isw, G, N1g, C, N1c, Ng, Nc, W(Lx0+1), W(Lxf+1)
     &, Nx, Xi2, W(Lmx+1), Jerr)
      Mo = 0
      Ierr   = Jerr
      IF (Ierr.NE.0)            GO TO 999
*
  110 CONTINUE
      Istop = 0.
      CALL USER (0, Isw, W(Lxf+1), Nx, C(1,1), N1c, Nc, Istop)
      Ierr   = 10 + Istop
      IF (Istop.GE.9)           GO TO 999
*		User forced Cut Step
      IF (Istop.eq.1)           GO TO 777
*
      Xi2 = DMBIL(Ng,W(Lxf+1),W(Lxf+2),W(Lcv11),W(Lcv12),W(Lcv21)
     &,     W(Lxf+1),W(Lxf+2))
*
      Xqua   = 0.
      DO 120 Ix = 1,Nx
        IF (Isw(Ix).EQ.0) GO TO 120
        ErX2    = W(Lmx+Nmx*(Ix-1)+Ix)
        ErX2    = max(ErX2,1.E-12)
        QWE = ABS(W(Lx0+Ix)-W(Lxf+Ix))
        QWE = MIN(QWE,1.E+6)
        QWE = QWE**2/Erx2
        Xqua    = max(Xqua,QWE)
 120    CONTINUE
        CQua   = 0.
        ICquaC = 0
        CSum   = 0.
        DO 140 Ic = 1,Nc
          If (Isw(Nx+Ic).EQ.0) GO TO 140
          CSum = Csum + ABS(C(1,Ic))
          CquaX   = 1.E+10
          JCquaX  = 0
          DO 160 Ix=1,Nx
          IF (Isw(Ix).EQ.0) GO TO 160
          TMP = C(Ix+1,Ic)
          IF (ABS(TMP).LE.1.E-10)       GO TO 160
          ErX2    = W(Lmx+Nmx*(Ix-1)+Ix)
          ErX2    = max(ErX2,1.E-12)
          TMP = ABS(C(1,Ic)/TMP)
          TMP = MIN(TMP,1.E+6)
          TMP = TMP**2/(ErX2*Goodsc(Ic))
          IF (TMP.GT.CquaX) GO TO 160
          CquaX = TMP
          JCquaX = Ix
  160     CONTINUE

        IF (JCquaX.EQ.0) GO TO 140
        IF (CquaX .LE.Cqua) GO TO 140
        Cqua = CquaX
        ICquaC = Ic
        ICquaX = JCquaX
  140   CONTINUE
*
      Tquaqq = 1.E+5 * CSumqq + Xi2qq
      Tqua   = 1.E+5 * CSum   + Xi2
*
  777 IOK = 777
      IF (Ncut.ne.0)            IOK = 0
      IF (Xqua.GT.1.1*Xquaqq)   IOK = 0
      IF (Xqua.GT.Good)         IOK = 0
      IF (Cqua.GT.Good)         IOK = 0
*		User forced Cut Step
      IF (Istop.eq.1)           IOK = 0

      IF (IOK.NE.0)             GO TO 900
*
      Ierr = 9
      IF (It.GT.5 .AND. Xi2.GT.Xi2max) GO TO 999     

*
**        Cut step?
      XXcut = Tqua .GT. Tquaqq *1.1
      IF (It.le.5) XXcut = .FALSE.
*		User forced Cut Step
      
      IF (Istop.eq.1)  XXcut = .TRUE.
      IF (Ncut.GT.Mxcut)  Then
        Ierr = 4
        IF (Istop.eq.1) GO TO 999
        XXcut = .FALSE.
        Tquaqq = 1.E+10
        Ncut = 0
      ENDIF

      IF (XXcut) THEN
*
        Ncut = Ncut + 1
        DO J=1,Nmx
          W(Lxf+J) = W(Lx0+j) +(W(Lxf+J)-W(Lx0+J))/2
        ENDDO
        GO TO 110
      ENDIF
      IF (Ncut.EQ.0) GO TO 100
      Ncut = 0
      GO TO 101

*
**       OK, we are here, fit is done
  900 Ierr  = 0
      DO I=1,Nx
        Xf(I) = W(Lxf+I)
        DO J=1,Nx
          Gf(I,J) = W(Lmx+Nmx*(J-1)+I)
        ENDDO
      ENDDO
*
  999 ISLAT(1) = It
      RETURN
      END
*
*DECK,CFITGO. ========================================================
      SUBROUTINE CFITGO (Init, Isw, G, N1g, C, N1c, Ng, Nc
     &, X0, Xf, Nx, Xi2, W, Ierr)
************************************************************************
* CFIT                                                                 *
*                                                                      *
*  Created     :  8-JUN-1994   Author : Victor Perevoztchikov          *
*                                                                      *
*  Function    :  One step of fit with constrains                      *
*                                                                      *
*  References  : None                                                  *
*                                                                      *
*  Arguments   :                                                       *
*                                                                      *
*    Init      0=continue
*              1= G is weight matrix
*              2 G is error  matrix
*    Isw       X & C switches (see CFIT)
*    G         error/weight  mtx                                       *
*    N1g       first dimension of G                                    *
*    C         mtx contains values of constrains & gradients in Xb+X0  *
*    N1c       first dimension of C                                    *
*    Ng        number of measured values, Ng <= N1g                    *
*    Nc        number of constrains, Nc+1 <= N1c                       *
*    X0        current values of X-Xb (Nx wds) + Nc wds of coefs
*    Xf        fitted  values of X-Xb (Nx wds) + Nc wds of coefs
*    Nx        size of X, Nx >= Ng                                     *
*    Xi2       Chisquare of fit                                        *
*    Gf        error matrix of fitted errors
*    N1gf      first dimension of GF
*    W         work array of size (Nx+Nc)*(Nx+Nc+1)+Ng*Ng              *
*    Ierr      error flag                                              *
*                                                                      *
*  Errors      :                                                       *
*    0 = Ok                                                            *
*    1 = bad error mtx (G)                                             *
*    2 = No solution                                                   *
************************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
*                                                                      *
* Formal parameters                                                    *
*____________________                                                  *
      DIMENSION      G(N1g,Ng),C(N1c,Nc), X0(Nx+Nc), Xf(Nx+Nc),W(*)
      INTEGER        Isw(Nx+Nc)
      REAL G,Xi2
*
*
* Global sections
*__________________
*
*
* Local variables
*__________________
      DOUBLE PRECISION X0,Xf,W,Det,DMBIL
      PARAMETER (NDBL=2)
*
*
* Executable statements
*________________________
*
      Lmx = 0
      Nmx = Nx + Nc
      Lcv = LMX + Nmx**2
      Lww = LCV + NG**2
*
      Lmx11 = Lmx+1
      Lmx21 = Lmx11+1
      Lmx12 = Lmx11+Nmx

      Lcv11 = Lcv+1
      Lcv21 = Lcv11+1
      Lcv12 = Lcv11+Ng
      Lww1 = Lww + 1
*
      IF (Init.NE.0)            THEN
        DO I1=1,Ng
        DO I2=1,Ng
          W(Lcv+Ng*(I2-1)+I1) = G(i1,i2)
        ENDDO
        ENDDO
        IF (Init.EQ.2)          THEN
          CALL DSINV (Ng, W(Lcv11), Ng, Ifail)
          Ierr = 1
          IF (Ifail.NE.0)       GO TO 999
        ENDIF
      ENDIF
*
**         Fill big Mx & right side
      CALL VZERO (Xf(1), Nmx*NDBL)
      CALL VZERO (W(Lmx11), Nmx**2*NDBL)
*

      CALL DMCPY (Ng, Ng, W(Lcv11), W(Lcv12), W(Lcv21)
     &,          W(Lmx11),W(Lmx12),W(Lmx21))
      DO 200 Ic = 1,Nc
        Xf(Nx+Ic) = -C(1,Ic)
        DO  210 Ix = 1,Nx
          W(LMX+Nmx*(Ix-1)+Nx+Ic) = C(Ix+1,Ic)
          W(LMX+Nmx*(Nx+Ic-1)+Ix) = C(Ix+1,Ic)
  210     CONTINUE
  200   CONTINUE
*
      DO I = 1,Nmx
        IF (Isw(I).EQ.0) THEN
           X0(I) = 0.
           Xf(I) = 0.
           CALL DVSET(Nmx,0.D0,W(Lmx+Nmx*(I-1)+1),W(Lmx+Nmx*(I-1)+2))
           CALL DVSET(Nmx,0.D0,W(Lmx+I),         W(Lmx+Nmx+I))
           W(Lmx+Nmx*(I-1)+I) = 1.          
        ENDIF
      ENDDO
*
      CALL DMMPA(Nc,Nx,W(Lmx+Nx+1),W(Lmx+Nmx+Nx+1),W(Lmx+Nx+2)
     &,X0(1),X0(2),Xf(Nx+1),Xf(Nx+2))

*
**         FIT
CCC      call DXTEST(6,'QWE',W,Nmx,Nmx,Nmx,IERR)
      CALL DFACT (Nmx, W(Lmx11), Nmx, W(Lww1), Ifail, Det, Jfail)
      Ierr = 2
      If (Ifail.ne.0) go to 999
      CALL DFEQN (Nmx, W(Lmx11), Nmx, W(Lww1), 1, Xf(1))
      
      CALL DFINV (Nmx, W(Lmx11), NmX, W(Lww1))
      DO Ix=1,Nx
        IF (Isw(Ix).EQ.0 .AND. Xf(Ix).NE.0.)
     &  WRITE(*,*) 'Wrong Xf',Ix,XF(Ix)
      ENDDO      
*
      Xi2 = DMBIL(Ng,Xf(1),Xf(2),W(Lcv11),W(Lcv12),W(Lcv21),Xf(1),Xf(2))

      Ierr = 0
*
  999 RETURN
      END
*DECK,CFITST. ========================================================
      SUBROUTINE CFITST (USER, Isw, Xf, Nx, C, N1c, Nc, Ierr)
************************************************************************
* CFITST                                                               *
*                                                                      *
*  Created     :  8-JUN-1994   Author : Victor Perevoztchikov          *
*                                                                      *
*  Function    :  Testing of user routine
*                                                                      *
*  References  : None                                                  *
*                                                                      *
*  Arguments   :                                                       *
*                                                                      *
*    USER      User routine
*
*    Isw       X & C switches(see CFIT)
*    Xf        fitted  values of X-Xb (Nx wds) + Nc wds of coefs
*    Nx        size of X, Nx >= Ng                                     *
*    C         mtx contains values of constrains & gradients in Xb+X0  *
*    N1c       first dimension of C                                    *
*    Nc        number of constrains, Nc+1 <= N1c                       *
*    W         work array of size (Nx+Nc)*(Nx+Nc+1)+Ng*Ng              *
*    Ierr      error flag                                              *
*                                                                      *
*  Errors      :                                                       *
*    0   = Ok                                                          *
*    >0  = wrong_constrain+1000*wrong_variable
************************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL *8        C(N1c,Nc), Xf(Nx)
      INTEGER        Isw(Nx+Nc)
      EXTERNAL       USER

      Jerr = 0
      Ierr = -1

      DO 100 Jc = 1,Nc
      IF (Isw(Nx+Jc).EQ.0) GO TO 100
      CALL USER (1, Isw, Xf(1), Nx, C(1,1), N1c, Nc, Istop)
      If (Istop.ne.0) RETURN
      Nacc = 0
      Csav = C(1,Jc)
      Do 110 Jx = 1,Nx
        IF (Isw(Jx).EQ.0) GO TO 110
        Xsav = Xf(Jx)
        Cx0 = C(1+Jx,Jc)
        Dx = 1.E-3
        IF (ABS(Cx0).GT.1.e-6) THEN
          Nacc = Nacc + 1
          Dx = 0.1*Csav/Cx0
          If (ABS(Dx).LE.1.e-4) Dx = 1.e-3
        ENDIF
        Ncut = 10
 130    Xf(jx) = Xsav + Dx
        CALL USER (1, Isw, Xf(1), Nx, C(1,1), N1c, Nc, Istop)
        If (Istop.ne.0) RETURN
        Cx1 = C(1+Jx,Jc)
*        If (ABS(Cx1-Cx0).LT.0.1*ABS(cx0) .OR. Ncut.LE.0) GO TO 150
        If (ABS(Cx1-Cx0).LE.0.1*ABS(cx0) .OR. Ncut.LE.0) GO TO 150
 140    Ncut = Ncut - 1
* Don't drive dx to zero
        If (ABS(Dx/2.0).LE.1.e-4) go to 150
        Dx = 0.5*Dx
        GO TO 130
*
 150    D1 = 0.5*(Cx1+Cx0)
        D2 = (C(1,jc)-Csav)/Dx
        IF (ABS(D1-D2).gt.0.01*(ABS(D1)+ABS(D2))) THEN
          IF (Ncut.GT.0) GO TO 140
          Jerr = Jx+1000*Jc
          WRITE(*,*) 'Wrong Deriv for',Jx,Jc,D1,D2
          WRITE(3,*) 'Wrong Deriv for',Jx,Jc,D1,D2
        ENDIF
        Xf(jx) = Xsav
        CALL USER (1, Isw, Xf(1), Nx, C(1,1), N1c, Nc, Istop)
        If (Istop.ne.0) RETURN
 110    CONTINUE
        If (Nacc.EQ.0) THEN
          Jerr = 1000*Jc
          WRITE(*,*) 'Empty Deriv for',Jx,Jc
          WRITE(3,*) 'Empty Deriv for',Jx,Jc
        ENDIF
 100    CONTINUE
*
      Ierr = Jerr
      RETURN
      END


*DECK,MXPRNT.
      SUBROUTINE MXPRNT(LUN,TIT,A,IDIM1,IROW,ICOL,NCOLP,FORMAT)
      DIMENSION A(IDIM1,*)
      CHARACTER *(*) FORMAT,TIT
      CHARACTER *8 FRMT(7)
      CHARACTER *56 F
      EQUIVALENCE (F,FRMT)
      DATA FRMT/'(1H0,I5,','0','0','/(1X,I5,','0','0','))'/
      FRMT(3)=FORMAT
      NCOL = NCOLP
      IF (NCOL.LE.0) NCOL = ICOL
      IF (FORMAT.EQ.' ' .OR. FORMAT.EQ.'*') THEN
        FRMT(3) = 'G10.2'
        NCOL = 10
      ENDIF
      FRMT(6)=FRMT(3)
      IF (TIT.NE.' ') WRITE(LUN,'(//6X,A/)') TIT
      DO 4 K=1,ICOL,NCOL
      L=K+NCOL-1
      IF(L-ICOL)3,3,2
    2 L=ICOL
    3 IT=L+1-K
      WRITE(FRMT(2),100) IT
      FRMT(5)=FRMT(2)
    4 WRITE(LUN,F)(I,(A(I,J),J=K,L),I=1,IROW)
      RETURN
  100 FORMAT(I8)
      END
*DECK,MXTEST.
      SUBROUTINE MXTEST(LUN,TIT,A,IDIM1,IROW,ICOL,IERR)
      DIMENSION A(IDIM1,*)
      CHARACTER *(*) TIT
      IERR = 0
      ITIT = 0
      IF (TIT.EQ.' ') ITIT = 1
      DO 100 K1=1,ICOL-1
      DO 110 K2=K1+1,ICOL
      DETMX = 0.
      DO 120 I1=1,IROW-1
      DO 130 I2=I1+1,IROW
        DET = ABS(A(I1,K1)*A(I2,K2)-A(I2,K1)*A(I1,K2))
        QWE = ABS(A(I1,K1)*A(I2,K2))+ABS(A(I2,K1)*A(I1,K2))
        IF (QWE.GT.0.) DET=DET/QWE
        IF (DET.GT.1.E-5) GO TO 110
        DETMX = MAX(DETMX,DET)
 130  CONTINUE
 120  CONTINUE
      IERR = 1
      IF (ITIT.EQ.0) WRITE(LUN,'(//6X,A/)') TIT
      WRITE(LUN,*)' LinDep for',K1,I1,K2,I2,DETMX
      ITIT = 1
 110  CONTINUE
 100  CONTINUE
      END
      SUBROUTINE DXTEST(LUN,TIT,A,IDIM1,IROW,ICOL,IERR)
      IMPLICIT REAL*8 (A-H,O-z)
      DIMENSION A(IDIM1,*)
      CHARACTER *(*) TIT
      IERR = 0
      ITIT = 0
      IF (TIT.EQ.' ') ITIT = 1
      DO 100 K1=1,ICOL-1
      DO 110 K2=K1+1,ICOL
      DETMX = 0.
      DO 120 I1=1,IROW-1
      DO 130 I2=I1+1,IROW
        DET = ABS(A(I1,K1)*A(I2,K2)-A(I2,K1)*A(I1,K2))
        QWE = ABS(A(I1,K1)*A(I2,K2))+ABS(A(I2,K1)*A(I1,K2))
        IF (QWE.GT.0.) DET=DET/QWE
        IF (DET.GT.1.E-5) GO TO 110
        DETMX = MAX(DETMX,DET)
 130  CONTINUE
 120  CONTINUE
      IERR = 1
      IF (ITIT.EQ.0) WRITE(LUN,'(//6X,A/)') TIT
      WRITE(LUN,*)' LinDep for',K1,I1,K2,I2,DETMX
      ITIT = 1
 110  CONTINUE
 100  CONTINUE
      END
