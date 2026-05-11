
      FUNCTION PHO_CT14PDF(Iparton,X,Q)

C============================================================================
C                CTEQ-TEA Parton Distribution Functions: version 2014
C                       May 14, 2015
C
C   Reference: S. Dulat, T.-J. Hou, J. Gao, M. Guzzi, J. Huston, P. M.
C   Nadolsky, J. Pumplin, C. Schmidt, D. Stump, C.-P. Yuan,  arXiv:
C
C   This package provides a standard interface for CT10 and CT14
C   parton distribution functions.
 
C   The following sets of CTEQ PDF table files can be computed
C    with this program:
C   (1) 1+50 sets of CT10 NNLO PDF's;
C   (2) 1+52 sets of CT10 NLO PDF's;
C   (3) 4 sets of CT10 NNLO and NLO PDF's with alternative alpha_s values;
C   (4) 1+56 sets of CT14 NNLO PDF's;
C   (5) 1+56 sets of CT14 NLO PDF's;
C   (6) 2 sets of CT14 LO PDF's;
 
C ===========================================================================
C   The table grids are generated for
C       10^-9 < x < 1 and 1.3 < Q < 10^5 (GeV).
C
C   PDF values outside of the above range are returned using extrapolation.
C
C   The Table_Files are assumed to be in the working directory.
C
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCT14(Iset)
C   where Tablefile is a 40-character text string with the name of the
C   the desired PDF specified in the above table.table (.pds) file
C
C   Other provided functions include:
C   The function pho_CT14Pdf (Iparton, X, Q)
C     returns the parton distribution inside the proton for parton [Iparton]
C     at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C     Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                              for (b, c, s, d, u, g, u_bar, ..., b_bar),
C
C   The function CT14Alphas (Q)
C     returns the value of the QCD coupling strength alpha_s(Q) at
C     an energy scale Q. The alpha_s value is obtained from the interpolation
C     of a numerical table of alpha_s included in each .pds file.
C     In the PDF fit, the alpha_s values are obtained at each momentum
C     scale by evolution in the HOPPET program at the respective QCD order
C     (NLO or NNLO). The table of alpha_s values at discrete Q values
C     is included in the input .pds file. The function CT14Alphas
C     estimates alpha_s at an arbitrary Q value, which agrees
C     with the direct evolution by HOPPET within a fraction of percent
C     point at typical Q.
C
C   The function CT14Mass(i)
C     returns the value of the quark mass for the i-th flavor.
C     The flavors are:
C     1  2  3  4  5  6
C     u  d  s  c  b  t
C
C   Values of various PDF parameters assumed in the computation of the
C    PDFs can be obtained by
C     Call CT14GetPars( xmin,Qini,Qmax,Nloops,Nfl),
C   which returns
C     xmin, the minimal value of x;
C     Qmin,  the initial Q scale for the PDF evolution;
C     Qmax,  the maximal Q scale included in the PDF table;
C     Nloop, the number of QCD loops (order of the PDF in the QCD coupling);
C     Nfl,   the maximal number of quark flavors assumed in the PDF and
C            alpha_s evolution.
 
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single
C   precision.
C   If you have detailed questions concerning these CT14 distributions,
C   or if you find problems/bugs using this package, direct inquires to
C   nadolsky@physics.smu.edu.
C
C===========================================================================

      IMPLICIT NONE
      DOUBLE PRECISION ALFaq , PHO_CT14PDF , PHO_PARTONX12 , Q , QALfa , 
     &                 qsml , X
      INTEGER IORder , Iparton , IPDsset , IPK , MXVal , NFL , NFMx , 
     &        NT , NX
C  input/output channels
      INCLUDE 'inc/poinou'
 
      LOGICAL warn
      INTEGER ISEtch , IPDsformat
      COMMON /CTQPAR2/ NX , NT , NFMx , MXVal/QCDTBL/ ALFaq , QALfa , 
     &                 IPK , IORder , NFL/SETCHANGE/ ISEtch , IPDsset , 
     &                 IPDsformat                 !for external use
 
      DATA warn/.TRUE./
      DATA qsml/.3D0/
      SAVE warn
 
      IF ( IPDsset.NE.1 )
     &      STOP 'pho_CT14Pdf: the PDF table was not initialized'
 
      IF ( X.LT.0D0 .OR. X.GT.1D0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &        'X out of range in pho_CT14Pdf: ' , X
         PHO_CT14PDF = 0D0
         RETURN
      END IF
 
      IF ( Q.LT.qsml ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &        'Q out of range in pho_CT14Pdf: ' , Q
         STOP
      END IF
 
      IF ( ABS(Iparton).GT.NFMx ) THEN
         IF ( warn ) THEN
C        print a warning for calling extra flavor
            warn = .FALSE.
            IF ( LPRi.GT.4 ) WRITE (LO,*)
     &            'Warning: Iparton out of range in pho_CT14Pdf! '
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'Iparton, MxFlvN0: ' , 
     &           Iparton , NFMx
         END IF
         PHO_CT14PDF = 0D0
      ELSE
 
         PHO_CT14PDF = PHO_PARTONX12(Iparton,X,Q)
         IF ( PHO_CT14PDF.LT.0D0 ) PHO_CT14PDF = 0D0
      END IF                    !if (abs(Iparton...
 
 
C                             ********************
      END FUNCTION
