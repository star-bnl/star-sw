CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C============================================================================
C                CTEQ Parton Distribution Functions: Version 4.6
C                             June 21, 1996
C                   Modified: 10/17/96, 1/7/97, 1/15/97
C                             2/17/97, 2/21/97
C                   Last Modified on April 2, 1997
C
C   Ref[1]: "IMPROVED PARTON DISTRIBUTIONS FROM GLOBAL ANALYSIS OF RECENT DEEP
C         INELASTIC SCATTERING AND INCLUSIVE JET DATA"
C   By: H.L. Lai, J. Huston, S. Kuhlmann, F. Olness, J. Owens, D. Soper
C       W.K. Tung, H. Weerts
C       Phys. Rev. D55, 1280 (1997)
C
C   Ref[2]: "CHARM PRODUCTION AND PARTON DISTRIBUTIONS"
C   By: H.L. Lai and W.K. Tung
C       MSU-HEP-61222, CTEQ-622, e-Print Archive: hep-ph/9701256
C       to appear in Z. Phys.
C
C   This package contains 13 sets of CTEQ4 PDFs. Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)  Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C Ref[1]
C   1    CTEQ4M   Standard MSbar scheme   0.116     298   202    cteq4m.tbl
C   2    CTEQ4D   Standard DIS scheme     0.116     298   202    cteq4d.tbl
C   3    CTEQ4L   Leading Order           0.132     236   181    cteq4l.tbl
C   4    CTEQ4A1  Alpha_s series          0.110     215   140    cteq4a1.tbl
C   5    CTEQ4A2  Alpha_s series          0.113     254   169    cteq4a2.tbl
C   6    CTEQ4A3            ( same as CTEQ4M )
C   7    CTEQ4A4  Alpha_s series          0.119     346   239    cteq4a4.tbl
C   8    CTEQ4A5  Alpha_s series          0.122     401   282    cteq4a5.tbl
C   9    CTEQ4HJ  High Jet                0.116     303   206    cteq4hj.tbl
C   10   CTEQ4LQ  Low Q0                  0.114     261   174    cteq4lq.tbl
C ---------------------------------------------------------------------------
C Ref[2]
C   11   CTEQ4HQ  Heavy Quark             0.116     298   202    cteq4hq.tbl
C   12   CTEQ4HQ1 Heavy Quark:Q0=1,Mc=1.3 0.116     298   202    cteq4hq1.tbl
C        (Improved version of CTEQ4HQ, recommended)
C   13   CTEQ4F3  Nf=3 FixedFlavorNumber  0.106     (Lam3=385)   cteq4f3.tbl
C   14   CTEQ4F4  Nf=4 FixedFlavorNumber  0.111     292   XXX    cteq4f4.tbl
C ---------------------------------------------------------------------------
C   
C   The available applied range is 10^-5 < x < 1 and 1.6 < Q < 10,000 (GeV) 
C   except CTEQ4LQ(4HQ1) for which Q starts at a lower value of 0.7(1.0) GeV.  
C   Lam5 (Lam4, Lam3) represents Lambda value (in MeV) for 5 (4,3) flavors. 
C   The matching alpha_s between 4 and 5 flavors takes place at Q=5.0 GeV,  
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C   
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq4(Iset) 
C   where Iset is the desired PDF specified in the above table.
C   
C   The function Ctq4Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton] 
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C      whereas CTEQ4F3 has, by definition, only 3 flavors and gluon;
C              CTEQ4F4 has only 4 flavors and gluon.
C   
C   For detailed information on the parameters used, e.q. quark masses, 
C   QCD Lambda, ... etc.,  see info lines at the beginning of the 
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single 
C   precision.
C   
C   If you have detailed questions concerning these CTEQ4 distributions, 
C   or if you find problems/bugs using this package, direct inquires to 
C   Hung-Liang Lai(Lai_H@pa.msu.edu) or Wu-Ki Tung(Tung@pa.msu.edu).
C   
C===========================================================================

      Function Ctq4Pdf (Iparton, X, Q2)
      Implicit Double Precision (A-H,O-Z)

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/


      Logical Warn
      Common
     > / CtqPar2 / Nx, Nt, NfMx
     > / QCDtable /  Alambda, Nfl, Iorder
     > / XQrange / Qini, Qmax, Xmin

      Data Warn /.true./
      save Warn
      Q2STA = Qini
      Q2FIN = Qmax
      Xmax = 1D0
 

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : Q^2 set to minimal value !',Q2
         ENDIF
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : X set to minimal value !',X
         ENDIF
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : X set to maximal value !',X
         ENDIF
      endif
      
      Q=SQRT(Q2)

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
        Print *, 'X out of range in Ctq4Pdf: ', X
        Stop
      Endif
      If (Q .lt. Alambda) Then
        Print *, 'Q out of range in Ctq4Pdf: ', Q
        Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
             Warn = .false.
             Print *, 'Warning: Iparton out of range in Ctq4Pdf: '
     &              , Iparton
         Endif
         Ctq4Pdf = 0D0
         Return
      Endif

      Ctq4Pdf = PartonX (Iparton, X, Q)
      if(Ctq4Pdf.lt.0.D0)  Ctq4Pdf = 0.D0

      Return

C                             ********************
      End
