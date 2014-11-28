
C **********************************************************************

      BLOCK DATA LEPTOD
      
      IMPLICIT NONE
C...Give sensible default values to switches and parameters.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LFLMIX/ CABIBO(4,4)
      REAL CABIBO
*DEC Station gave a warning      SAVE /CABIBO/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      REAL    XR,QR,FLQT,FLGT,FLMT
      INTEGER NFX,NFQ
      SAVE /FLGRID/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      INTEGER I
      
C...LEPTOU: Cuts, basic switches and parameters.
      DATA CUT/1.E-04,1.,0.,1.,4.,1.E+08,5.,1.E+08,1.,1.E+08,1.,1.E+08,
     &0.,3.1416/
C...            0    1    2    3    4    5    6    7    8    9
      DATA LST/      0,   1,   5,   1,   3,   1,   1,  12,   5,
     1          1,   0,   4,   5,   4,   9,   1,   0,   2, -10,
     2          5,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     3          0,   0,   0,   0,   1,   1,   0,   0,   1,   0,
     4          0 /
C...            0    1    2    3    4    5    6    7    8    9
      DATA PARL/     1.,  1.,  0.44,0.75,.2319,0., 0.5, .04, 4.,
     1         0.0, 0.01, 0.01,0.1, 0.35,0.01,7.29735E-03,
     &                                   1.16639E-05,0.044,0.03,
     2         0.1,10*0./
C...Internally used variables.
      DATA PARI/50*0./
      DATA QC/-.33333,.66667,-.33333,.66667,-.33333,.66667,
     &        -.33333,.66667/
      DATA CABIBO/.95,.05,2*0.,.05,.948,.002,2*0.,.002,.998,4*0.,1./
      DATA OPTX/1.,3*0./,OPTY/1.,3*0./,OPTQ2/1.,3*0./,OPTW2/1.,3*0./
      DATA NXX,NWW/31,21/
      DATA PQG,PQQB,QGMAX,QQBMAX/6510*0./,YCUT/651*0./,XTOT/651*1.0E-12/
      DATA NFX,NFQ/41,16/,FLQT,FLGT,FLMT/1968*0./
      DATA XKIN/1.,2.,3.,4./,UKIN,WKIN,AIN,BIN/16*0./,MAXFIN/2000/
      DATA RELUP,RELERR,RELER2/0.1,0.05,0.05/
      DATA NAMKIN/'         x','          ','          ','          '/
      DATA IPY/
     1 0,     0,     2,     2,     6,     1,     1,     6,     3,     1,
     2 3,     1,     1,     2,     1,     1,     4,     1,     1,     1,
     3 0,     1,     1,     1,     1,     1,     1,     0,     0,     0,
     4 1,     2,     1,     1,    30,    33,     1,     1,     7,     0,
     5 0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     6 0,     0,     0,     1,   100,     0,     0,     0,     0,     0,
     7 0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     8 0,     0,     0,     0,     0,     0,     0,     0,     0,     0/
      DATA (PYPAR(I),I=1,40)/
     1   7.299E-03,   2.290E-01,   2.000E-01,   2.500E-01,   4.000E+00,
     1   1.000E+00,   4.400E-01,   4.400E-01,   7.500E-02,   0.000E+00,
     2   2.000E+00,   0.000E+00,   1.000E+00,   0.000E+00,   3.000E+00,
     2   1.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   1.000E+00,
     3   2.500E-01,   1.000E+00,   2.000E+00,   1.000E-03,   1.000E+00,
     3   1.000E+00,   1.000E+00,  -2.000E-02,  -1.000E-02,   0.000E+00,
     4   0.000E+00,   1.600E+00,   0.500E+00,   0.200E+00,   3.894E-01,
     4   1.000E+00,   3.300E-01,   6.600E-01,   0.000E+00,   1.000E+00/
      DATA (PYPAR(I),I=41,80)/
     5   2.260E+00,   1.000E+04,   1.000E-04,   0.000E+00,   0.000E+00,
     5   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     6   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     6   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     7   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     7   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     8   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     8   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00/
      DATA PYVAR/80*0./

      DATA IPLST/0,11,12,7*0/
      DATA CPDFNAM /
     1     './StarDb/Generators/Pepsi/pdf/MRSa1.dat','./StarDb/Generators/Pepsi/pdf/polnloA.dat', 
     2     './StarDb/Generators/Pepsi/pdf/MRSa1.dat','./StarDb/Generators/Pepsi/pdf/polnloB.dat',
     3     './StarDb/Generators/Pepsi/pdf/MRSa1.dat','./StarDb/Generators/Pepsi/pdf/polnloC.dat',
     4     ' ','./StarDb/Generators/Pepsi/pdf/polparA.dat',' ','./StarDb/Generators/Pepsi/pdf/polparB.dat',
     6     ' ','./StarDb/Generators/Pepsi/pdf/polparC.dat',
     7     ' ','./StarDb/Generators/Pepsi/pdf/STDNLO.GRID',' ','./StarDb/Generators/Pepsi/pdf/VALNLO.GRID',
     9     ' ','./StarDb/Generators/Pepsi/pdf/STDLO.GRID',' ','./StarDb/Generators/Pepsi/pdf/VALLO.GRID',
     1     ' ','./StarDb/Generators/Pepsi/pdf/S1NLO.GRID',
     2     ' ','./StarDb/Generators/Pepsi/pdf/S2NLO.GRID',
     3     ' ','./StarDb/Generators/Pepsi/pdf/S3NLO.GRID',
     4     ' ','./StarDb/Generators/Pepsi/pdf/S1LO.GRID',
     5     ' ','./StarDb/Generators/Pepsi/pdf/S2LO.GRID',
     6     ' ','./StarDb/Generators/Pepsi/pdf/S3LO.GRID',
     7     './StarDb/Generators/Pepsi/pdf/cteq4lq.tbl',' ',
     8     './StarDb/Generators/Pepsi/pdf/MRSasq.dat','./StarDb/Generators/Pepsi/pdf/MRSalq.dat',
****** grsv with grv98 a unpol refenrence |bs>*********************
     9     './StarDb/Generators/Pepsi/pdf/grv98lo.grid','./StarDb/Generators/Pepsi/pdf/std2000_lo_g1.grid',           
     1     './StarDb/Generators/Pepsi/pdf/grv98lo.grid','./StarDb/Generators/Pepsi/pdf/val2000_lo_g1.grid',
     2     './StarDb/Generators/Pepsi/pdf/grv98nlo.grid','./StarDb/Generators/Pepsi/pdf/std2000_nlo_g1.grid',
     3     './StarDb/Generators/Pepsi/pdf/grv98nlo.grid','./StarDb/Generators/Pepsi/pdf/std2000_nlo_g1.grid',
****** CTEQ 5 unpol ***********************************************
     4     './StarDb/Generators/Pepsi/pdf/cteq5l.tbl',' ',
     5     './StarDb/Generators/Pepsi/pdf/cteq5m.tbl',' ',
     6     './StarDb/Generators/Pepsi/pdf/cteq5m1.tbl',' ',
****** MRS 99 unpol ***********************************************
     7     './StarDb/Generators/Pepsi/pdf/cor01.dat',' ',
     8     './StarDb/Generators/Pepsi/pdf/cor02.dat',' ',
     9     './StarDb/Generators/Pepsi/pdf/cor03.dat',' ',
     1     './StarDb/Generators/Pepsi/pdf/cor04.dat',' ',
     2     './StarDb/Generators/Pepsi/pdf/cor05.dat',' ',
     3     './StarDb/Generators/Pepsi/pdf/cor06.dat',' ',
     4     './StarDb/Generators/Pepsi/pdf/cor07.dat',' ',
     5     './StarDb/Generators/Pepsi/pdf/cor08.dat',' ',
     6     './StarDb/Generators/Pepsi/pdf/cor09.dat',' ',
     7     './StarDb/Generators/Pepsi/pdf/cor10.dat',' ',
     8     './StarDb/Generators/Pepsi/pdf/cor11.dat',' ',
     9     './StarDb/Generators/Pepsi/pdf/cor12.dat',' ', 
     1     './StarDb/Generators/Pepsi/pdf/cteq6l.tbl',' ',
     2     './StarDb/Generators/Pepsi/pdf/cteq6d.tbl',' ',
     3     './StarDb/Generators/Pepsi/pdf/cteq6m.tbl',' ' /
      DATA IPDFNAM /
     1     107,1,108,2,109,3,
     4     113,4,114,5,115,6,
     7     116,7,117,8,118,9,119,10,
     1     124,11,125,12,126,13,127,14,128,15,129,16,
     7     133,17,137,18,144,19,145,20,146,21,
     1     147,22,150,23,151,24,152,25,161,26,
     2     162,27,163,28,164,29,165,30,166,31,167,32,
     3     168,33,169,34,170,35,171,36,172,37,173,38,174,39,175,40 /

      END
