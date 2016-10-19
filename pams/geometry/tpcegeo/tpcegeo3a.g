!// $Id: tpcegeo3a.g,v 1.2 2011/02/24 15:48:20 jwebb Exp $
!// $Log: tpcegeo3a.g,v $
!// Revision 1.2  2011/02/24 15:48:20  jwebb
!// Volumes TBRW (Aluminum) and TWAS (Air) overlap.  Changed positioning of
!// TBRW so that it is positioned as an ONLY volume, to remove ambiguity in
!// geant tracking.  The change has no effect on an AgSTAR geometry, see
!//
!// http://drupal.star.bnl.gov/STAR/node/20519
!//
!// Revision 1.1  2011/02/10 18:47:06  jwebb
!// This is a code-cleanup of tpcegeo3.g.  Significant changes, mainly to
!// remove equivalence statements which are not permitted in AgML.
!//
!// Revision 1.24  2010/01/13 22:36:24  perev
!// Typo is fixed. TPCG was twice as bigger
!//
!// [snip]
!//
!// Revision 1.4  2008/08/27 21:48:17  fisyak
!//
!// TPC


MODULE     tpceGeo3a  is the updated TPC

  Author   David Underwood corrected by Maxim Potekhin, 
           implmented in TGeo by Yuri, 
           translated back to AgSTAR by Victor,
           and code cleanup by Jason.  Phew.

  Created  Dec 7, 2005

  +cde,AGECOM,GCUNIT,GCONST.     " - standard geant commons "
  CHARACTER *4 voluName
  Content   TPCE,TOFC,TOFS,TOST,TOKA,TONX,TOAD,TOHA,TPGV,TPSS,
            TIFC,TIAL,TIKA,TINX,TPCW,TWSS,TWGI,TPCM,TPEA,TESS,
            TSWH,TMWC,TMEA,TMSE,TIAG,TOAE,TPAD,TPAI,TPAO,THOL,
            THRA,THLA,TALC,TAEC,TCEX,TCRX,TSAW,TWGC,TWGB,TPIP,
            TMAN,TRDV,TRDS,TRDC,TIAD,TOIG,FEES,FEEP,FEER,FEEI,
            FEEA,TSAS,TWAS,TALS,TSGT,TWBT,TWRC,TWRG,TWRI,TWTR,
            TWMR,TRDO,TBRW,TWRB,TCOO,TCAB,TRIB,TWIR

  INTEGER   kInner, kOuter 
  PARAMETER ( kInner=0 )
  PARAMETER ( kOuter=1 )

  REAL INCH ,CM
  PARAMETER (INCH=2.54,CM=1.);

  INTEGER kBlack,kRed,kGreen,kBlue,kYellow,kViolet,kLightBlue;
  PARAMETER (kBlack=1,kRed=2,kGreen=3,kBlue=4);
  PARAMETER (kYellow=5,kViolet=6,kLightBlue=7);

  REAL sind,cosd,tand                    ! EXTERNAL FUNCTIONS
  REAL cos15,sin15,tan15;
  PARAMETER (cos15 =.965925826289068312) !// Cos(15./180*Pi);
  PARAMETER (sin15 =.258819045102520739) !// Sin(15./180*Pi);
  PARAMETER (tan15 =.267949192431122696) !// Tan(Pi*15./180);

  INTEGER i,j,iSecAng,nCount/0/,jTRIB,kase;

  REAL x,x0,x1,x2,dx,dx1,dx2,xc;
  REAL y,y0,y1,y2,dy,dy1,dy2;
  REAL z,z0,z1,z2,dz,dz1,dz2;
  REAL zBeg,zDed,zPmt,zEnd;
  REAL r,r0,r1,r2,dr;
  REAL alpha,beta;
  REAL xw,dxw,yw,dyw,zw,dzw,dYdX
  REAL dRSectorShift/0/
  REAL myDx,myDy,myDz,myAlph,myThet,myPhi,myPar(20)

  !// radii of outer finest structureures
  REAL tocsIR,tocsOR,tokaIR,tokaOR,tonxIR ,tonxOR ,toadIR,toadOR,toigIR,toigOR;
  REAL toalIR,toalOR,tohaIR,tohaOR,toalIR2,toalOR2,tofcIR,tofcOR,tofsIR,tofsOR;

  !// radii of inner finest structureures
  REAL tifcIR,tialIR,tialOR,tikaIR,tikaOR,tinxIR,tinxOR,tiadIR,tiadOR,tifcOR;

  !//   variables from tpcegeo2
  REAL tpgvLeng,tofcLeng,tpgvz;
  INTEGER i_sec,i_row;

  EXTERNAL  TPADSTEP,TPAISTEP,TPAOSTEP,TPCELASER

  !//  derive radii of larger structureures
  REAL    tpgvIR; !// TPC gas inner radius

  !//  make primitive control and basic printout
  REAL del;

  REAL zWheel1,zTOFCend ,zTIFCend,zTIFCFlangeBegin;
  REAL qwe(10);

  
  REAL xHoles(0:15);


  REAL width1IOF(2),
       width2IOF(2),
       heightIOF(2),
       depthIOF(2) 

  !//   Inner/Outer index
  INTEGER inOut;

  !//     Tpc(Inner/Outer)SectorAssembly
  REAL zzzSA(2)         
  REAL rmnSA(0:1)     
  REAL rmxSA(0:1)
  DATA zzzSA /207.4   ,219.1 /
  DATA rmnSA /51      ,121.07/
  DATA rmxSA /121.07  ,194   /

  !//     Tpc(Inner/Outer)WheelAssembly
  REAL zzzWA(2)        
  REAL rmnWA(0:1)       
  REAL rmxWA(0:1) 
  DATA zzzWA /211.4,229.2/
  DATA rmnWA /54   ,121  /
  DATA rmxWA /121  ,189  /

  !//             TpcRDOAssembly
  REAL zzzRDO(2)
  REAL rmnRDO(2)
  REAL rmxRDO(2)
  DATA zzzRDO/232. ,252./;
  DATA rmnRDO/ 88. , 88./;
  DATA rmxRDO/198. ,198./;

  !//             OuterRing
  REAL myCos,myRmin,myRmax,myG1,myG2;

  !// RDO & cool
  REAL dRDOCooling(0:6) 
  DATA dRDOCooling/16.0, 16.0, 16.0, 16.7, 15.0, 3.5, 14.5/
  REAL RCoolingTube/123456789/;
  CHARACTER *4 mySha;
  INTEGER iCoo,iCab,jCoo,iRib,iTALS,jTALS;
  
  
  
  !// TPC Main geometry parameters
  Structure TPCG { version,Rmin,Rmax,RminIFC,
       LengthT,Length,LengthW,LengthV,dZEnvelop,WheelIR,
       WheelR0,WheelR1,WheelR2,WheelTotalRibWidth,WheelRibWidth,
       WheelRibHeight,WheelBoxDx,WheelBoxDy,WheelOR1,WheelOR,
       WheelTHK1,WheelTHK,SenGasOR,MembTHK,tocsDR,
       tokaDR,tonxDR,toadDR,toigDR,toalDR,
       tohaDR,tiadDR,tinxDR,tikaDR,tialDR,
       tifcRF,tifcDRT,dzYF1,dzYF2,dzYF3,
       PadPlaneThickness,dGateGround,WireMountWidth,
       WireMountHeight,dxRDO,dyRDO,dzRDO,zRDO,
       heigTube,widTube,RDOCoolingdX,RDOCoolingdY,RDOCoolingdZ,
       zGatingGrid,zGroundGrid,DeadZone,dAnode(2) }


  !//  sector of padrows
  Structure TPRS { sec,Nrow,pitch,width,super,dAnode,Rpads(40),Npads(40) }

  !// EC trapezoid and support Wheel
  structure TECW {sec, GapWidI, GapWidO, GapHeit, GapRad, inwidth, ouwidth,
       widthLip, noribs, zStepRib, widthRib, Height, Thick,
       ThickAl, rMin, Rcenter,holeDx,holeDy}

  Structure TPCR { RdoVthk,Rdothk,Rdolen,NRdobrd,Rdoht(9) }

  !//     FEE stuff
  Structure TFEE {Vers,CardDX ,CardDY,CardDZ,PlateDX,PlateDY,PlateDZ,
       AssemblyThickness,RibDX ,RibDY  ,RibDZ, Ass(3) }


  Structure RIBS { version,  inout,    num,
       x(16),    dx(16),   y(16),        
       dy(16)    }


  Structure HOLZ { int inout, int numberOfRows, int numberPerRow(16), yholes(160) }    

  Structure COOL { int inout, tubeLength(16) }
   
  Fill  TPCG !//   TPC basic dimensions
     version =       3               !// version    => current version
     Rmin =          46.107          !// Rmin          => TPC envelope inner radius
     Rmax =          206.75    !// Rmax          => TPC envelope outer radius
     RminIFC =       46.6            !// RminIFC    => inner radius TPC IFC  : A.Lebedev measurement 10/16/08
     LengthT =       2*271.0         !// LengthT    => TPC full length up to front of EEMC
     Length  =       2*259.685       !// Length        => TPC full length including RDOs
     LengthW =       2*229.71        !// LengthW    => TPC length including Wheel
     LengthV =       2*210.00        !// LengthV    => TPC gas volume length
     DzEnvelop =     268.            !// TPCE envelop dz
     WheelIR =       38.620*INCH/2   !// 49.60 WheelIR    => support wheel inner radius
     WheelR0 =       21.500*INCH     !// WheelR0 => Distance from IP of end of inner cylindrical part
     WheelR1 =       47.867*INCH     !// WheelR1 => Distance from IP of middle Rib
     WheelR2 =       76.093*INCH     !// WheelR2 => Distance from IP of upper Rib
     WheelTotalRibWidth =7.6         !// WheelTotalRibWidth => total rib width (A.Lebedev measurement)
     WheelRibWidth = 1.9             !// WheelRibWidth         => rib plate width (A.Lebedev measurement)
     WheelRibHeight =1.3             !// WheelRibHeight        => rib plate height (A.Lebedev measurement)
     WheelBoxDx =    4.7             !// WheelBoxDx (A.Lebedev measurement)
     WheelBoxDy =    7.6             !// WheelBoxDy (A.Lebedev measurement)
     WheelOR1 =      201.0           !// WheelOR1   => @ TOFC
     WheelOR =       206.75          !// WheelOR    => support wheel outer radius
     WheelTHK1 =     5.72            !// WheelTHK1
     WheelTHK =      11.43           !// WheelTHK   => support wheel length
     SenGasOR =      200             !// SenGasOR   => TPC sensitive gas outer radius
     MembTHK =       0.00762         !// MembTHK    => Central membrane thickness
     tocsDR =        0.013           !// tocsDR        => outer copper thickness:  NIM A499 (2003) 659-678 Table 2
     tokaDR =        0.015           !// tokaDR        => outer kapton thickness
     tonxDR =        0.953           !// tonxDR        => outer nomex thickness
     toadDR =        0.05            !// toadDR        => outer adhesive thickness
     toigDR =        5.70            !// toigDR        => outer isolating gas thickness
     toalDR =        0.40            !// toalDR        => outer aluminum thickness (for both layers)
     tohaDR =        0.60            !// tohaDR        => outer Honeycomb Al thickness
     tiadDR =        0.080           !// tiadDR        => inner adhesive layer thickness:  NIM A499 (2003) 659-678 Table 2
     tinxDR =        1.270           !// tinxDR        => inner nomex structureure thickness
     tikaDR =        0.015           !// tikaDR        => inner Kapton layer thickness
     tialDR =        0.004           !// tialDR        => inner aluminum layer thickness
     tifcRF =        51.7            !// tifcRF        => outer radius of IFC flange
     tifcDRT=        0.1             !// tifcDRT    => tolerance between flange and wheel
     dzYF1   =       221.162         !// dz of YF1
     dzYF2  =        211.9           !// dz of YF2
     dzYF3  =        208.02          !// dz of YF3
     PadPlaneThickness = 0.182       !// 1.82 mm  Drawing 24A0314 & 24A0304
     dGateGround = 0.6            !//
     WireMountWidth  = 5*((0.130+0.1376)/2)*INCH  !//
     WireMountHeight = 1.340*inch    !// Drawing 24A0434
     dxRDO   = 1.75/2                !// A.Lebedev 1 RDO = 5 lb
     dyRDO   = 45./2                 !//
     dzRDO   = 17.0/2                !//
     zRDO    = TPCG_LengthW/2 + 20.0         !//
     heigTube = 0.703*INCH           !// x Cooling TUBE Drawing 24A0801B
     widTube  = 0.500*INCH           !// z Mixture TPCE_Water_Pipe => rho = 2.32155 g/cm**3
     RDOCoolingdX = (38.0 + 9.0 + 58.0)/2    !//
     RDOCoolingdY = 1.25                     !//
     RDOCoolingdZ = 2.50                     !//
     dAnode = { 0.2 , 0.4 }                  !// Inner/Outer anode width
     zGatingGrid = TPCG_LengthV/2 -(TPCG_dGateGround+2*TPCG_dAnode(2)) 	!// 
     zGroundGrid = TPCG_zGatingGrid+TPCG_dGateGround       		!//
     DeadZone      = 12. !// Dead zone before GatingGrid. No hits there
  EndFill

  USE TPCG

  Fill TPRS                    !// sector of padrows
     sec    = 1                !// sector number: 1 for inner, 2 for outer
     nRow   = 13               !// number of padrows in the sector
     pitch  = 0.335            !// tpc padrow pitch width
     width  = 1.15             !// tpc padrow thickness
     super  = 3                !// number of padraws in a superpadrow
     dAnode = 0.2              !// distance to anode wire from pad plane
     Npads  = { 88, 96, 104, 112, 118, 126, 134, 142, 150,
               158 , 166, 174, 182 }        !// number of pads in row
     Rpads  = {60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8,
               104.0,109.2,114.4,119.6 }    !// tpc padrow radii
  EndFill
  
  Fill TPRS                    !// sector of padrows
     sec    = 2                !// sector number: 1 for inner, 2 for outer
     nRow   = 32               !// number of padrows in outer sector
     pitch  = 0.67             !// outer tpc padrow pitch width
     width  = 1.95             !// outer tpc padrow thickness
     super  = 1                !// number of padrows in a superpadrow
     dAnode = 0.4              !// distance to anode wire from pad plane
     Npads  = { 98, 100, 102, 104, 106, 106, 108, 110, 112,
               112 , 114, 116, 118, 120, 122, 122, 124, 126,
               128 , 128, 130, 132, 134, 136, 138, 138, 140,
               142 , 144, 144, 144, 144 } !// number of pads in row
     Rpads  = {127.195, 129.195, 131.195, 133.195, 135.195,
               137.195, 139.195, 141.195, 143.195, 145.195,
               147.195, 149.195, 151.195, 153.195, 155.195,
               157.195, 159.195, 161.195, 163.195, 165.195,
               167.195, 169.195, 171.195, 173.195, 175.195,
               177.195, 179.195, 181.195, 183.195, 185.195,
               187.195, 189.195 }        !// tpc padrow radii
  EndFill
  
  USE TPRS

  !//  TECW_t TECW[2] = !// EC trapezoid and support Wheel
  Fill TECW       !// EC trapezoid and support Wheel
     sec          =         1             !// sec   => sector number: 1 for inner, 2 for outer
     GapWidI      =         2.* 10.91     !// GapWidI  => air in support wheel - inner width
     GapWidO      =         2.* 27.56     !// GapWidO  => air in support wheel - outer width
     GapHeit      =         62.15         !// GapHeit  => air in support wheel - Height (dr)
     GapRad       =         87.0  !// GapRad   => air in support wheel - center radius
     inwidth      =         10.829*INCH   !//2.* 13.75,   !// inwidth  => sector width at inner radius
     ouwidth      =         25.498*INCH   !//2.* 32.38,   !// ouwidth  => sector width at outer radius
     widthLip     =         ((10.829-9.852)+(25.498-24.521))/4*INCH,      !// 1.24079  widthLip =>  width of lip
     noribs       =         11            !// noribs
     zStepRib     =         3.150*INCH    !// zStepRib
     widthRib     =         1.47*COS(15./180*ACOS(-1.))*INCH !// widthRib => side rib width
     Height       =         27.373*INCH   !// Drawing 24A3685B 27.77*INCH from drawing 24A0325,
     !// => 69.527 => (Wire Plane)  69.52,  Height   => sector radial Height
     Thick        =         3.300*INCH    !// 24A3685B  3.219*INCH        !// Thick    => sector thickness
     thickAl      =         0.375*INCH    !// " 9.525mm ThickAl  => Thick - air gap"
     rMin         =         51.905        !// rMin     => Minimum distance to beam line (wire plane)
     Rcenter      =         86.669        !// Rcenter  => sector center radius (set by precision holes)
     holeDx       =         0.750/2*INCH  !// = 0.9525 cm
     holeDy       =         2.625/2*inch  !// = 3.33375 cm
  EndFill

  Fill TECW       !// EC trapezoid and support Wheel
     sec          =         2             !// sec    => sector number: 1 for inner, 2 for outer
     GapWidI      =         2.* 28.92     !// GapWidI  => air in support wheel - inner width
     GapWidO      =         2.* 46.74     !// GapWidO  => air in support wheel - outer width
     GapHeit      =         65.0          !// GapHeit  => air in support wheel - Height (dr)
     GapRad       =         158.0         !// aGapRad   => ir in support wheel - radius
     inwidth      =         25.561*INCH   !//64.92,       !// inwidth  => sector width at inner radius
     ouwidth      =         40.649*INCH   !//103.25,      !// ouwidth  => sector width at outer radius
     widthLip     =         ((25.561-24.583)+(40.649-39.671))/4*INCH      !// = 1.242060 widthLip =>  width of lip
     noribs       =         14            !// noribs
     zStepRib     =         3.150*INCH    !// zStepRib
     widthRib     =         1.47*COS(15./180*ACOS(-1.))*INCH      !// widthRib = > side rib width
     Height       =         28.155*INCH   !// 71.514 (Wire Plane) => 71.51,       !// Height   => sector radial Height
     Thick        =         3.140*INCH    !// 24A3925G thick    => sector thickness
     ThickAl      =         0.375*INCH    !// "= 9.525 mm; ThickAl  => Thick - air gap"
     rMin         =         121.732       !// rMin     => Minimum distance to beam line (wire plane)
     Rcenter      =         157.488       !// Rcenter  => sector center radius (set by precision holes)
     holeDx       =         0.750/2*INCH  !//
     holeDy       =         2.500/2*INCH  !//
  EndFill
  
  Fill TPCR              !// volume for tpc readout boards
     RdoVthk   = 30.     !// length of RDO volume
     Rdothk    =.25      !// thickness of rdo card
     Rdolen    = 27      !// card length along beam direction
     NRdobrd   = 9       !// number of RDO boards
     Rdoht = {60.0, 74.0, 84.0, 101.0,106.0,
              126.0,146.0,166.0,186.0} !// radial pos of rdo boards
  EndFill
  
  Fill TFEE                               !// frontEndElectronics
     Vers    = 1                          !// version of FrontEndElectronics
     CardDX  = 1.47*0.110*INCH/2          !// 1.47 scale factor account for 9 lb of cables
     CardDY  = 2.900*INCH/2               !//...
     CardDZ  = 7.000*INCH/2               !//...
     PlateDX = 0.110*INCH/2               !//...
     PlateDY = 1.480*INCH                 !//...
     PlateDZ = 4.650*INCH/2               !//...
     AssemblyThickness =    TFEE_CardDX + TFEE_PlateDX    !//...
     RibDX   = 0.820*INCH/2 - 2*   TFEE_CardDX            !//...
     RibDY   = 2.900*INCH/2 - 2*   TFEE_CardDX            !//...
     RibDZ   = TFEE_CardDX                !//...
     Ass  = {1.6, 4, 10}                  !// Fee assembly brik size
  EndFill


  USE TECW sec=kInner+1
  FILL RIBS !// Rib structure / inner  Drawing 24A3925G
     version=kInner !// version number
     inout  =kInner !// selctor
     num    =11     !// number of elements 
     x      ={  0.000, 4.261, 10.561, 16.861, 23.161, 
               25.950, 3.726,  3.726, 10.026, 10.026, 
               25.480 } !// x
     y      ={  0.000, 0.000,  0.000, 0.000, 0.000, 0.000, 
               -5.645, 5.645, -3.895, 3.895, 0.000        } !// y  
     dx     ={ 1.470, 0.375, 0.375 , 0.375 ,0.375 ,TECW_Height/INCH-25.950, 
               1.070, 1.070, 1.070, 1.070, 0.940 }!// dx  ... last=2*(25.950-(1.050+2.515+21.915)) 
     dy     ={ 0.,0.,0.,0.,0.,0.,0.750,0.750,0.750,0.750,2.0} !// dy 
  ENDFILL

  USE TECW sec=kOuter+1
  FILL RIBS !// Rib structure / outer  Drawing 24A3925G
     version=kOuter !// version number
     inout  =kOuter !// selctor
     num    =14     !// number of elements 
     x  = {  0.280,  4.261,  7.410, 10.560, 13.710, 16.859,
            20.009, 23.158, 26.309,  6.023,  6.023, 12.3225,
            15.472, 26.309-1.575/2} !// x
     y  = { 0,0,0,0,0,0,0,0,0,17.250/2,-17.250/2,0,0,0}!// y
     dx = { 1.268, 0.375,  0.375, 0.375, 0.375, 0.375,
            0.375, 0.375,  1.566, 2.774, 2.774, 2.775, 
            2.774, 1.575 } !// dx
     dy = { 0,0,0,0,0,0,0,0,0,1.250, 1.250, 0.375, 0.375,1.5680}!// dy
  ENDFILL


  Fill HOLZ !// holes per row inner
     inout = kInner  !// selector

     numberOfRows = 15 !// Number of rows
     numberPerRow = { 6, 5, 5, 2, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 0 } !// n/row

     yholes = { 3.180*(-2.5+0), 3.180*(-2.5+1), 3.180*(-2.5+2), 3.180*(-2.5+3), 3.180*(-2.5+4), 
                3.180*(-2.5+5), 3.180*(-2.5+6), 3.180*(-2.5+7), 3.180*(-2.5+8), 
                3.764*(-2.0+0), 3.764*(-2.0+1), 3.764*(-2.0+2), 3.764*(-2.0+3), 3.764*(-2.0+4), 
                3.764*(-2.0+5), 3.764*(-2.0+6), 3.764*(-2.0+7), 3.764*(-2.0+8), 
                3.553*(-2.0+0), 3.553*(-2.0+1), 3.553*(-2.0+2), 3.553*(-2.0+3), 3.553*(-2.0+4), 
                3.553*(-2.0+5), 3.553*(-2.0+6), 3.553*(-2.0+7), 3.553*(-2.0+8), 
               13.368*(-0.5+0),13.368*(-0.5+1),13.368*(-0.5+2),13.368*(-0.5+3),13.368*(-0.5+4),
               13.368*(-0.5+5),13.368*(-0.5+6),13.368*(-0.5+7),13.368*(-0.5+8),
                3.131*(-2.0+0), 3.131*(-2.0+1), 3.131*(-2.0+2), 3.131*(-2.0+3), 3.131*(-2.0+4), 
                3.131*(-2.0+5), 3.131*(-2.0+6), 3.131*(-2.0+7), 3.131*(-2.0+8),
               -3.892 - 1.948 , -1.948        , +1.948        , 3.892 + 1.948 , 0.000         , 
                0.000         , 0.000         , 0.000         , 0.000         ,
               -3.611 - 1.807 , -1.807        , +1.807        , 3.611 + 1.807 , 0.000         , 
                0.000         , 0.000         , 0.000         , 0.000         ,
               -3.281 - 1.7145, -1.7145       , +1.7145       , 3.281 + 1.7145, 0.000         , 
                0.000         , 0.000         , 0.000         , 0.000         ,
                3.150*(-1.5+0), 3.150*(-1.5+1), 3.150*(-1.5+2), 3.150*(-1.5+3), 3.150*(-1.5+4), 
                3.150*(-1.5+5), 3.150*(-1.5+6), 3.150*(-1.5+7), 3.150*(-1.5+8),
                4.152*(-1.0+0), 4.152*(-1.0+1), 4.152*(-1.0+2), 4.152*(-1.0+3), 4.152*(-1.0+4), 
                4.152*(-1.0+5), 4.152*(-1.0+6), 4.152*(-1.0+7), 4.152*(-1.0+8),
                3.730*(-1.0+0), 3.730*(-1.0+1), 3.730*(-1.0+2), 3.730*(-1.0+3), 3.730*(-1.0+4), 
                3.730*(-1.0+5), 3.730*(-1.0+6), 3.730*(-1.0+7), 3.730*(-1.0+8),
                3.308*(-1.0+0), 3.308*(-1.0+1), 3.308*(-1.0+2), 3.308*(-1.0+3), 3.308*(-1.0+4), 
                3.308*(-1.0+5), 3.308*(-1.0+6), 3.308*(-1.0+7), 3.308*(-1.0+8),
                3.075*(-1.0+0), 3.075*(-1.0+1), 3.075*(-1.0+2), 3.075*(-1.0+3), 3.075*(-1.0+4), 
                3.075*(-1.0+5), 3.075*(-1.0+6), 3.075*(-1.0+7), 3.075*(-1.0+8),
                4.927*(-0.5+0), 4.927*(-0.5+1), 4.927*(-0.5+2), 4.927*(-0.5+3), 4.927*(-0.5+4), 
                4.927*(-0.5+5), 4.927*(-0.5+6), 4.927*(-0.5+7), 4.927*(-0.5+8),
                4.083*(-0.5+0), 4.083*(-0.5+1), 4.083*(-0.5+2), 4.083*(-0.5+3), 4.083*(-0.5+4), 
                4.083*(-0.5+5), 4.083*(-0.5+6), 4.083*(-0.5+7), 4.083*(-0.5+8),
                0.000         , 0.000         , 0.000         , 0.000         , 0.000         , 
                0.000         , 0.000         , 0.000         , 0.000                 } !// y holes
  EndFill!// HOLZ

  Fill HOLZ !// holes per row outer
  inout = kOuter  !// selector
  numberOfRows = 16 !// Number of rows
  numberPerRow = { 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 6 } !// n/row
  yholes       = { -15.025, -11.606, -8.177, -4.220,  0,  4.220,  8.177,  11.606, 15.025,
                   -15.025, -11.606, -8.177, -4.220,  0,  4.220,  8.177,  11.606, 15.025,
                   -14.243, -11.078, -6.330, -3.165,  0,  3.165,  6.330,  11.078, 14.243,
                   -14.243, -11.078, -6.330, -3.165,  0,  3.165,  6.330,  11.078, 14.243,
                   -13.452, -10.023, -6.330, -3.165,  0,  3.165,  6.330,  10.023, 13.452,
                   -13.452, -10.023, -6.330, -3.165,      3.165,  6.330,  10.023, 13.452, 0,
                   -12.660,  -9.495, -6.330, -2.110,      2.110,  6.330,   9.495, 12.660, 0,
                   -12.660,  -9.495, -6.330, -2.110,      2.110,  6.330,   9.495, 12.660, 0,
                   -11.870,  -8.440, -5.275, -2.110,      2.110,  5.275,   8.440, 11.870, 0,
                   -11.870,  -8.440, -5.275, -2.110,      2.110,  5.275,   8.440, 11.870, 0,
                   -11.078,  -7.649, -4.220,          0,  4.220,  7.649,  11.078,      0, 0,
                   -11.078,  -7.649, -4.220,          0,  4.220,  7.649,  11.078,      0, 0,
                   -10.022,  -6.858, -3.429,          0,  3.429,  6.858,  10.022,      0, 0,
                   -10.022,  -6.858, -3.429,          0,  3.429,  6.858,  10.022,      0, 0,
                   -9.495,   -6.330, -3.165,          0,  3.165,  6.330,   9.495,      0, 0,
                   -9.495,   -6.330, -3.165,              3.165,  6.330,   9.495,  0,  0, 0 }!// y holes
  EndFill


  FILL COOL !// Cooling tube lengths / inner
  inout = kInner !// Inner vs outer
  tubeLength = { 19.300, 18.454, 17.610, 16.766, 15.922,
                 15.078, 14.235, 13.390, 12.546, 11.704,
                 10.860, 10.016,  9.172,  8.327,  7.483, 
                  0.000 }!// Tube lengths
  ENDFILL

  FILL COOL !// Cooling tube lengths / outer
  inout = kOuter !// Inner vs outer
  tubeLength = { 33.620, 33.605, 32.761, 31.917,
                 31.073, 30.229, 29.385, 28.541,
                 27.697, 26.853, 26.009, 25.165,
                 24.321, 23.478, 22.634, 21.790 }!// Tube lengths
  ENDFILL

  
  USE TFEE

  USE TECW
  USE TPCR
  USE TPCG

  !//
  !// TPC basic dimensions are the full system size, the gas volume length
  !// and inner radius are derived from them and from material thicknesses.
  !// The outer gas radius is also an input ( as this is used for winding ),
  !// but the remaining clearance is checked to be positive (PN, 16 Mar 96).
  !//
  !// layer names mnemonic:
  !// -----------------------------------------------------------------------
  !//  letter 1-6 :   t  :     i/o     :  cs/ka/nx/al/fc/fs  :     IR/OR    :
  !// -----------------------------------------------------------------------
  !//    meaning  :  TPC : inner/outer :    materials as     :  inner/outer :
  !//             :      :     cage    : Copper shild,Kapton :    radius    :
  !//             :      :             : Nomex,Aluminum  OR  :    of the    :
  !//             :      :             : field cage and      :    layer     :
  !//             :      :             : field cage support  :              :
  !// -----------------------------------------------------------------------
  !//

  tofcLENG = tpcg_Length-2*tpcg_WheelTHK-2*TPCR_RdoVthk  ! gas plus endcaps
  tpgvLeng = (tofcLeng-tpcg_MembTHK)/2   	! active gas
  tpgvLeng = (TPCG_LengthV-tpcg_MembTHK)/2 	! active gas


  ! calculate radii of outer finest structureures
  tocsIR = TPCG_SenGasOR;
  tocsOR = tocsIR + TPCG_tocsDR;
  tokaIR = tocsOR; tokaOR = tokaIR + TPCG_tokaDR;
  tonxIR = tokaOR; tonxOR = tonxIR + TPCG_tonxDR;
  toadIR = tonxOR; toadOR = toadIR + TPCG_toadDR;

  toigIR = toadOR; toigOR = toigIR + TPCG_toigDR;
  toalIR = toigOR; toalOR = toalIR + TPCG_toalDR/2.;
  tohaIR = toalOR; tohaOR = tohaIR + TPCG_tohaDR;
  toalIR2= tohaOR; toalOR2= toalIR2+ TPCG_toalDR/2.;

  tofcIR = tocsIR; tofcOR = toalOR2;
  tofsIR = tocsIR; tofsOR = toadOR;

  ! calculate radii of inner finest structureures
  tifcIR = TPCG_RminIFC;
  tialIR = tifcIR; tialOR = tialIR + TPCG_tialDR/2.;
  tikaIR = tialOR; tikaOR = tikaIR + TPCG_tikaDR;
  tinxIR = tikaOR; tinxOR = tinxIR + TPCG_tinxDR;
  tiadIR = tinxOR; tiadOR = tiadIR + TPCG_tiadDR;
  tifcOR = tiadOR + TPCG_tialDR/2.;

  tpgvIR = tifcOR;         !// TPC gas inner radius
  
  !//  make primitive control and basic printout
  del = TPCG_Rmax-tofcOR;

  Prin0 TPCG_version; (' TPCE: config=',F9.3)
  Prin1 tofcOR, del ; (' TPCE: max radius=',F9.3,' clearance=',F9.3);

  width1IOF = { 7.661*inch, 22.287*inch};
  width2IOF = {20.552*inch, 35.422*inch};
  heightIOF = {24.222*inch, 24.840*inch};
  depthIOF  = { 1.800*inch,  1.800*inch};

  !//   nomex - PN  8/3/96 following I.Sacreda note
  COMPONENT C      A=12  Z=6  W=5
  COMPONENT H      A=1   Z=1  W=8
  COMPONENT O      A=16  Z=8  W=2
  MIXTURE   Nomex  Dens=0.064

  COMPONENT C       A=12    Z=6     W=0.625
  COMPONENT H       A=1     Z=1     W=0.4166667E-01
  COMPONENT O       A=16    Z=8     W=0.3333333
  MIXTURE   Adhesive    dens=1.2

  COMPONENT Al        A=27  Z=13   W=0.0105
  COMPONENT N         A=14  Z=7    W=0.7395
  COMPONENT Adhesive  A=9   Z=4.5  W=0.2500
  MIXTURE   Al_honeycomb    dens=0.282

!   G10 is given as 60% SiO2 and 40% epoxy in ftpcgeo.g, from which
!   the following is taken
  COMPONENT  Si  A=28.08  Z=14  W=0.6*1*28./60.
  COMPONENT  O   A=16     Z=8   W=0.6*2*16./60.
  COMPONENT  C   A=12     Z=6   W=0.4*8*12./174.
  COMPONENT  H   A=1      Z=1   W=0.4*14*1./174.
  COMPONENT  O   A=16     Z=8   W=0.4*4*16./174.
  MIXTURE G10  Dens=1.7
  MATERIAL  G10

  COMPONENT  H  A=1  Z=1  W=1.06546299984001044e-02
  COMPONENT  O  A=16 Z=8  W=8.52370399872008355e-02
  COMPONENT  AL A=27 Z=14 W=9.04108330014399053e-01
  MIXTURE Water_Pipe  Dens=2.32155
  MATERIAL  Water_Pipe


  COMPONENT  CU  A=63.54   Z=29  W=0.586            "// Copper"
  COMPONENT  C   A=12.01   Z=6   W=0.259            "// Carbon"
  COMPONENT  O   A=15.999  Z=8   W=0.138            "// Oxigen"
  COMPONENT  H   A=1.00794 Z=1   W=0.017            "// Hydrogen"
  MIXTURE Cables  Dens=2.68
  MATERIAL  Cables

!     TPC default gas P10: Ar/methane 9:1 by volume
  COMPONENT Ar    A=40  Z=18 W=9
  COMPONENT C     A=12  Z=6  W=1
  COMPONENT H     A=1   Z=1  W=4
  MIXTURE P10  Dens=0.154053E-02


!__________________________________________ Geomety ______________________________________


   Create and Position TPCE in CAVE

!_________________________________________________________________________________________

BLOCK TPCE is the TPC envelope

      MATERIAL  Air
      MEDIUM    Standard
      Attribute TPCE  seen=0 colo=kRed
      SHAPE     TUBE  rmin=tpcg_rmin  rmax=tpcg_rmax  dz=tpcg_DzEnvelop

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      tpgvz  = (tpcg_MembTHK + tpgvLeng)/2               " z center of gas volume   "
      zWheel1  = TPCG_LengthW/2 - TPCG_WheelTHK;        !// write(*,*) 'zWheel1 ', zWheel1;
      zTOFCend = TPCG_LengthW/2 - TPCG_WheelTHK1;       !// write(*,*) 'zTOFCend', zTOFCend; !// end of TOFC
      zTIFCend = TPCG_LengthW/2 + 0.97;                 !// write(*,*) 'zTIFCend', zTIFCend; !// end of TIFC + flange
      zTIFCFlangeBegin = zTIFCend - 1*INCH;             !// write(*,*) 'zTIFCFlangeBegin' , zTIFCFlangeBegin;

      Create and position  TPGV z=+tpgvz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0,
                                konly='many'
                 position  TPGV z=-tpgvz,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0,
                                konly='many'
                               

      Create and position  TPCM       "    membrane    "

      Create and position TIFC        "   inner cage   "
      Create and position TOFC        "   outer cage   "

      DO iSecAng = 0,360-30,30
         Create and Position TSWH            alphaz=iSecAng kOnly='MANY'
         Create and Position TSWH ort = XY-Z alphaz=iSecAng kOnly='MANY'
      ENDDO

endBlock !// end TPCE


!=============================================================================
BLOCK TPCM is the Central Membrane placed in TPC
      MATERIAL  Mylar
      Attribute TPCM  seen=1    colo=kBlue
!*    MEDIUM    dense_membrane  SteMax=1  " just to show that we define it "
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpcg_MembTHK/2
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
endBlock  "end TPCM"


!=============================================================================
BLOCK TIFC  defines the Inner Field Cage placed in TPC
! Contents of Inner Field Cage structureure TIFC: Adhesive,Nomex,Kapton,Aluminum
!=============================================================================
!?    MATERIAL   MYLAR (changed by YF)

      MATERIAL   ALUMINIUM
      Attribute TIFC   seen=1  colo=kBlue
      SHAPE     PCON    Phi1=0  Dphi=360  Nz=6,
      zi ={-zTIFCend,-zTIFCFlangeBegin,-zTIFCFlangeBegin, zTIFCFlangeBegin,zTIFCFlangeBegin,zTIFCend},
      Rmn={tifcIR,tifcIR,tifcIR,tifcIR,tifcIR,tifcIR},
      Rmx={TPCG_tifcRF,TPCG_tifcRF,tifcOR,tifcOR,TPCG_tifcRF,TPCG_tifcRF}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      Create and position TIKA
      Create and position TINX
      Create and position TIAD

endBlock        "end TIFC"


!=============================================================================
BLOCK TIKA is the kapton film of the inner field cage
      MATERIAL  Mylar
      Attribute TIKA   seen=1  colo=kViolet
      SHAPE     TUBE   rmin=tikaIR rmax=tikaOR dz=TPCG_dzYF1

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TIKA"


!=============================================================================
BLOCK TINX is the inner nomex structureure
      MATERIAL  Nomex
      Attribute TINX   seen=1  colo=kYellow
      SHAPE     TUBE   rmin=tinxIR rmax=tinxOR dz=TPCG_dzYF1

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TINX"


!=============================================================================
BLOCK TIAD the inner adhesive structureure
      MATERIAL  ADHESIVE
      Attribute TIKA   seen=1  colo=kViolet
      SHAPE     TUBE   rmin=tiadIR rmax=tiadOR dz=TPCG_dzYF1

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TIAD"


!=============================================================================
BLOCK TOFC  defines outer field cage - fill it WITH insulating gas already

      MATERIAL  ALUMINIUM
      Attribute TOFC   seen=1 colo=kGreen
      SHAPE     PCON   Phi1=0  Dphi=360  Nz=6,
            zi  ={-zTOFCend, -zWheel1,-zWheel1,zWheel1,zWheel1,zTOFCend},
            Rmn ={TPCG_WheelOR1,TPCG_WheelOR1,tofcIR,tofcIR,TPCG_WheelOR1,TPCG_WheelOR1},
            Rmx ={TPCG_WheelOR ,TPCG_WheelOR ,tofcOR,tofcOR,TPCG_WheelOR ,TPCG_WheelOR }

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      create and position TOAD

      create and position TOFS
      create and position TOKA
      create and position TONX
      create and position TOIG
      create and position TOHA

endBlock        "end TOFC"


!=============================================================================
BLOCK TOFS  is the Outer Field Cage structureure
! Contents of Outer Field Cage structureure TOFS:  Copper/Kapton/Nomex/Adhesive
      MATERIAL  COPPER
      Attribute TOFS      seen=1  colo=kBlue
      SHAPE     TUBE      rmin=tocsIR rmax=tocsOR dz=TPCG_dzYF2

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TOFS"


!=============================================================================
BLOCK TOKA  is  MYLAR layer
! Contents of Outer Field Cage structureure TOFS:  Copper/Kapton/Nomex/Adhesive
      MATERIAL  mylar     " should be close to kapton "
      Attribute TOKA      seen=1  colo=kGreen
      SHAPE     TUBE      rmin=tokaIR  rmax=tokaOR dz=TPCG_dzYF2

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TOKA"


!=============================================================================
BLOCK TONX  is  Nomex support
      MATERIAL  Nomex
      Attribute TONX   seen=1  colo=kRed
      SHAPE     PCON   Phi1=0  Dphi=360  Nz=6,
                       zi ={-TPCG_dzYF2, -TPCG_dzYF3,-TPCG_dzYF3,TPCG_dzYF3,TPCG_dzYF3,TPCG_dzYF2},
                       Rmn={tonxOR-0.2 ,tonxOR-0.2  ,tonxIR     ,tonxIR    ,tonxOR-0.2,tonxOR-0.2},
                       Rmx={tonxOR     ,tonxOR      ,tonxOR     ,tonxOR    ,tonxOR    ,tonxOR    }

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TONX"


!=============================================================================
BLOCK TOAD  is  Adhesive layer
      MATERIAL  mylar     " should be close ? "
      Attribute TOAD      seen=1  colo=kLightBlue
      SHAPE     TUBE      rmin=toadIR rmax=toadOR dz=TPCG_dzYF2

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TOAD"


!=============================================================================
BLOCK TOIG  is Insulating Gas (Nitrogen)
      MATERIAL  NITROGEN_GAS
      Attribute TOAD      seen=1  colo=kLightBlue
      SHAPE     TUBE      rmin=toigIR rmax=toigOR dz=TPCG_dzYF2

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TOIG"


!=============================================================================
BLOCK TOHA  Gas Containment Vessel (Al) + HA
      MATERIAL  AL_HONEYCOMB
      Attribute TOHA      seen=1  colo=kViolet
      SHAPE     TUBE      rmin=tohaIR rmax=tohaOR dz=TPCG_dzYF3

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TOHA"


!=============================================================================
!=============================================================================


!=============================================================================
BLOCK TSWH  TpcSectorWhole is Sector as Whole
      MEDIUM  STANDARD
      Attribute TSWH      seen=1  colo=kViolet
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=8,
      zi ={TPCG_MembTHK/2,zWheel1                 ,zWheel1                 ,zTOFCend,
           zTOFCend      ,zTIFCFlangeBegin        ,zTIFCFlangeBegin        ,TPCG_DzEnvelop},
      Rmn={tifcOR        ,tifcOR                  ,tifcOR                  ,tifcOR,
           tifcOR        ,TPCG_tifcRF+TPCG_tifcDRT,TPCG_tifcRF+TPCG_tifcDRT,TPCG_tifcRF+TPCG_tifcDRT},
      Rmx={tofcIR        ,tofcIR                  ,TPCG_WheelOR1           ,TPCG_WheelOR1,
           TPCG_WheelOR  ,TPCG_WheelOR            ,TPCG_WheelOR            ,TPCG_WheelOR}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      Create and Position TSAW Konly='Many'

endBlock        "end TSWH"


!=============================================================================
BLOCK TSAW  TpcSectorAndWheel

      MEDIUM  STANDARD
      Attribute TSAW      seen=1  colo=kYellow
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=2,
      zi = {204,TPCG_DzEnvelop-TPCG_MembTHK/2}, Rmn={48,48}, Rmx={208,208}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
      
      dX2 = 1.470;
      dX  = 0;

      USE TECW sec=1;

      DO inOut = kInner, kOuter

         USE COOL inout=inOut
         USE RIBS inout=inOut
         USE HOLZ inout=InOut
         USE TECW sec=(inOut+1)
     
         z2 = zWheel1;
         z1 = z2 - TECW_Thick;
         z  = (z1 + TPCG_MembTHK/2)/2;
         dz = (z1 - TPCG_MembTHK/2)/2;

         x1 = -TECW_Height/2;
         x2 =  TECW_Height/2;
         y1 =  TECW_inwidth/2;
         y2 =  TECW_ouwidth/2;
         x0 =  (y1*x2-y2*x1)/(x2-x1);

         dYdX  =  (y2 - y1)/(x2 - x1);
         alpha = 180./Pi*ATAN(dYdX);

         r1 = y1/dYdX;
         r2 = y2/dYdX;

         dRSectorShift = TECW_rMin - r1;

         IF (inOut == kInner) THEN
            xHoles(0) = r2 - 1.988*INCH;

            DO i = 1, HOLZ_numberOfRows-1
               xHoles(i) = xHoles(i-1) - 1.575*INCH;
            ENDDO
         ELSE

            DO i = 0, HOLZ_numberOfRows - 1
               xHoles(i) = r2 - (1.988 + 1.575*i)*INCH
            ENDDO
         ENDIF

         Create And Position TSAS kOnly='MANY'
         Create And Position TWAS kOnly='MANY'
         Create And Position TBRW X=TPCG_WheelR1 Z=zWheel1+TPCG_WheelTHK/2      

         x1 = TPCG_WheelR0*cos15;
         x2 = TPCG_WheelR2;
         dz = TPCG_WheelRibHeight/2;
         z  = zWheel1 + dz;
         dx = (x2 - x1)/2;
         dy = TPCG_WheelTotalRibWidth/4;
         x  = (x1 + x2)/2;
         y  = (x *tan15 - dy)*(2*inOut-1);

         myDx=dy; myDy=dx; myDz=dz; myAlph=-(1-2*inOut)*15;

         Create  And Position TWRB x=x y=y z=z ORT=YX-Z

         dz = TPCG_WheelTHK/2;
         z  = zWheel1 + dz;
         dy = TPCG_WheelRibWidth/4;
         x  = (x1 + x2)/2;
         y  = (x *tan15 - dy)*(2*inOut-1);

         myDx=dy; myDy=dx; myDz=dz; myAlph=-(1-2*inOut)*15;

         Create  And Position TWRB x=x, y=y, z=z ORT=YX-Z


      ENDDO


      DO alpha = -15,15,30

         DO j = 0,3

            IF (j == 0) r = TPCG_WheelR2 - 1.5/8.5*(TPCG_WheelR2 - TPCG_WheelR1);
            IF (j == 1) r = TPCG_WheelR2 - 6.5/8.5*(TPCG_WheelR2 - TPCG_WheelR1);
            IF (j == 2) r = TPCG_WheelR1 - 2.0/8.5*(TPCG_WheelR1 - TPCG_WheelR0);
            IF (j == 3) r = TPCG_WheelR1 - 7.0/8.5*(TPCG_WheelR1 - TPCG_WheelR0);
            x = r ;
            y = r *tand(alpha);
            Position TBRW x=x y=y z=zWheel1+TPCG_WheelTHK/2 alphaz=alpha 

         ENDDO

         qwe(1) = -qwe(1);
         qwe(2) = -qwe(2);

      ENDDO

      Create And Position TWBT                      "  WheelBottom"
      Create And Position TWRI      kOnly='MANY'    "  WheelOuterRing"
      Create And Position TWTR                      "  pcWheelTopRib"
      Create And Position TWMR                      "  pcWheelMidleRib"
      Create And Position TRDO      kOnly='MANY'    "  RDOAssembly"


      
      RCoolingTube = TPCG_WheelR2 - TPCG_heigTube - 2*TPCG_dxRDO;
      DO iCoo = 0,7

         dz = TPCG_heigTube*cm/2;
         dy = TPCG_widTube *cm/2;
         z  = RCoolingTube;
         dx1 = (z-dz)*tan15 - 2*TPCG_RDOCoolingdY;
         dx2 = (z+dz)*tan15 - 2*TPCG_RDOCoolingdY;

         mySha='TRD1';
         myPar = {dx1,dx2,dy,dz};
         Create and Position TCOO x=z z=(TPCG_zRDO-2*dy) ORT=YZX
         RCoolingTube -= dRDOCooling(iCoo);

      ENDDO

      dz = 4.5;
      dy = 4.5/2;
      z  = TPCG_zRDO + TPCG_dzRDO + dz;
      x1 = TPCG_WheelR0;
      x2 = TPCG_WheelR2;
      dx = (x2 - x1)/2;
      x  = (x1 + x2)/2;
      DO iCab = 0,1

         y  = - (x *tan15 - dy) * (1 - 2*iCab);
         myPar={dy, dx, dz,-15*(1 - 2*iCab), 0, 0};

         Create and Position TCAB x=x y=y z=z ORT=YX-Z

      ENDDO

endBlock        "end TSAW"


!=============================================================================
BLOCK TSAS  TpcInnerSectorAssembly &  TpcOuterSectorAssembly TSAS and TSA1

      SHAPE  PGON    Phi1=-15  Dphi=30  Nz=2, nPDV=1,
             zi ={zzzSA(1),zzzSA(2)}, 
             Rmn={rmnSA(inOut),rmnSA(inOut)},
             Rmx={rmxSA(inOut),rmxSA(inOut)}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);


      Attribute TSAS  seen=0 

      Create and Position TALS         "!//TpcSectorAlSupport"
      Create and Position TSGT         "!//TpcSectorG10 (GTen)"

      ! mmmm.... Ribs

      dy = (r2 - r1)/2;
      y  = (r2 + r1)/2;
      dx = TECW_widthRib/2;
      dz = (TECW_Thick - TPCG_PadPlaneThickness - TECW_ThickAl)/2;
      z  = z2 - dz;
      dxW = TPCG_WireMountWidth/cos15/2;
      dzW = TPCG_WireMountHeight/2;
      xW = 0;

      DO iRib = 0,1

         mySha = 'PARA';
         myPar = {dx, dy, dz, -(1-2*iRib)*alpha};
         Create TRIB;

         x  = ((y1 + y2)/2 - dx)*(1-2*iRib);
         Position TRIB x=y y=-x z=z ORT=YX-Z

         xW = ((y1 + y2)/2 -  dxW)*(1-2*iRib);

         myPar = {dxW, dy, dzW, -(1-2*iRib)*alpha, 0, 0};
         Create TWIR;
         zW = z1 - dzW/2;

         Position TWIR x=y y=-xW z=zW ORT=YX-Z

      ENDDO

      DO iRib = 0, TECW_noribs-1

         r   = r2 - RIBS_x  (irib+1) * INCH
         y   =      RIBS_y  (irib+1) * INCH
         dr  =      RIBS_dx (irib+1) * INCH
         dy  =      RIBS_dy (irib+1) * INCH

         IF (dy < 1.e-7) THEN

            dx2 =  r      *dYdX - TECW_widthRib - 2*dxW; ! TECW_widthLip;
            dx1 = (r - dr)*dYdX - TECW_widthRib - 2*dxW; ! TECW_widthLip;
            xc = r - TECW_zStepRib/2;

            mySha='TRD1';
            myPar = {dx1, dx2, dz, dr/2};

            Create And Position TRIB x=r-dr/2 y=0 z=z2-dz ORT=YZX

         ELSE 

            mySha='BOX ';
            myPar = {dr/2, dy/2, dz};

            Create And Position TRIB x=r y=y z=z2-dz

         ENDIF

      ENDDO

endBlock        "end TSAS"


!=============================================================================
BLOCK TWAS  TpcWheelInnerAssembly & TpcWheelOuterAssembly     TWAS and TWA1

      SHAPE PGON  Phi1=-15  Dphi=30  Nz=2 nPDV=1,
                  zi ={zzzWA(1),zzzWA(2)}, 
                  Rmn={rmnWA(inOut),rmnWA(inOut)},
                  Rmx={rmxWA(inOut),rmxWA(inOut)}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      Attribute TWAS  seen=0 

      dx =   TPCG_widTube*cm/2;
      dz =   depthIOF(inOut+1)/2;
      dy =   heightIOF(inOut+1)/2;

      y1 = - dy;
      y2 =   dy;
      x1 =  width1IOF(inOut+1)/2 - dx;
      x2 =  width2IOF(inOut+1)/2 - dx;
      x0 =  (y1*x2-y2*x1)/(y2-y1);

      dYdX  =  (x2 - x1)/(y2 - y1);
      beta = 180./Pi*ATAN(dYdX);

      IF (inOut == kOuter) dy -= 0.7;
      y  = xHoles(0) - dy;
      x = (x1 + x2)/2;
      DO iCoo = 0,1

         myPar={dx, dy, dz,-(1-2*iCoo)*beta, 0, 0};
         mySha = 'PARA';
         Create  and Position TCOO  x=y y=x*(2*iCoo-1) z=zWheel1+dz ORT=YX-Z;

      ENDDO

      DO icoo = 0, HOLZ_numberOfRows - 1

         x = xHoles(iCoo);         !// cm

         dx = TPCG_heigTube*cm/2;
         dz = TPCG_widTube*cm/2;
         dy = COOL_tubeLength( icoo+1 )*INCH / 2

         IF (inOut == kInner) THEN
            dy2 = dy + 2*dYdX*dx;
            dy1 = dy;                  
         ELSE  
            dy2 = dy 
            dy1 = dy - 2*dYdX*dx
         ENDIF

         mySha='TRD1';
         myPar= {dy1, dy2, dz, dx};
         myPar(11) = x-dx-TFEE_AssemblyThickness/2+dRSectorShift;
         myPar(13) = zWheel1+dz;
         Create And Position TCOO x=myPar(11) z=myPar(13) ORT=YZX


         DO jcoo = 0, HOLZ_numberPerRow(icoo+1) - 1

            y = HOLZ_yholes( 9*icoo + jcoo + 1 )*INCH;

            Create And Position FEEA X=x+TFEE_AssemblyThickness/2+dRSectorShift,
            Y=y Z=zWheel1+2*dz+TFEE_RibDZ Konly='Many'

         ENDDO

      ENDDO

endBlock        "end TWAS"


!=============================================================================
BLOCK FEEA  TGeoVolumeAssembly(FEE)

      Attribute FEEA      seen=1  colo=kGreen
      SHAPE     BOX    dX=TFEE_Ass(1), dY=TFEE_Ass(2), dZ=TFEE_Ass(3)
      Attribute FEEA  seen=0 

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      Create And Position FEEP
      Create And Position FEER x=-TFEE_RibDX-TFEE_CardDX
      Create And Position FEEI x=2*TFEE_CardDX z=0.5

endBlock        "end FEEA"


!=============================================================================
BLOCK FEEP  FEEplate
      Attribute FEEP      seen=1  colo=kRed
      MATERIAL ALUMINIUM
      SHAPE     BOX    dX=TFEE_PlateDX dY=TFEE_PlateDY dZ=TFEE_PlateDZ

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end FEEP"


!=============================================================================
BLOCK FEER  FEERib
      Attribute FEER      seen=1  colo=1
      MATERIAL ALUMINIUM
      SHAPE     BOX    dX=TFEE_RibDX  dY=TFEE_RibDY  dZ=TFEE_RibDZ

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end FEER"


!=============================================================================
BLOCK FEEI  FEEitself
!// TGeoVolume *FEEitself= gGeoManager->MakeBox("FEEitself", GetMed("TPCE_G10"),       FeeCardDX, FeeCardDY, FeeCardDZ);
      Attribute FEEI      seen=1  colo=kViolet
      MATERIAL G10
      SHAPE     BOX  dX=TFEE_CardDX dY=TFEE_CardDY dZ=TFEE_CardDZ

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end FEEI"


!=============================================================================
BLOCK TALS TpcSectorAlSupport  the Sector G10 alliminium support

      Attribute TALS seen=1  colo=kYellow;
      MATERIAL ALUMINIUM;
      SHAPE PGON    Phi1=-alpha  Dphi=2*alpha  Nz=2, nPDV=1,
      zi = {z1+TPCG_PadPlaneThickness,z1+TPCG_PadPlaneThickness+TECW_ThickAl},
      Rmn={r1,r1}, Rmx={r2,r2};

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      myPar(1)  = TECW_holeDx;
      myPar(2)  = TECW_holeDy;
      myPar(3)  = TECW_ThickAl/2;

      myPar(13) = zWheel1 - TECW_Thick + TPCG_PadPlaneThickness + TECW_ThickAl/2;


      DO iTALS = 0, HOLZ_numberOfRows - 1

         myPar(11) = xHoles(iTALS); !// cm

         DO jtals = 0, HOLZ_numberPerRow( itals+1 ) - 1

            myPar(12) = HOLZ_yholes( 9*itals+jtals+1) *INCH

            Create And Position THOL x=myPar(11) y=myPar(12) z=myPar(13)

         ENDDO

      ENDDO

endBlock        "end TALS"


!=============================================================================
BLOCK TSGT TpcSectorG10    the Sector G10

      Attribute TSGT      seen=1  colo=kBlue
      MATERIAL G10
      SHAPE PGON  Phi1=-alpha  Dphi=2*alpha  Nz=2 nPDV=1,
            zi = {z1,z1+TPCG_PadPlaneThickness},
            Rmn={r1,r1} Rmx={r2,r2}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TSGT"


!=============================================================================
BLOCK TWBT //WheelBottom

      Attribute TWBT      seen=1  colo=kGreen
      MATERIAL  ALUMINIUM
      SHAPE PCON    Phi1=-15  Dphi=30  Nz=4,
              zi ={zWheel1,zWheel1+3.500*INCH,zWheel1+3.500*INCH,TPCG_LengthW/2  },
              Rmn={TPCG_WheelIR ,TPCG_WheelIR,TPCG_WheelIR+3.6  ,TPCG_WheelIR+3.6},
              Rmx={TPCG_WheelR0 ,TPCG_WheelR0,TPCG_WheelR0      ,TPCG_WheelR0    }

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);


endBlock        "end TWBT"

!=============================================================================
BLOCK TWRI      WheelOuterRing

      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=4,
                zi ={zWheel1      ,zTOFCend     ,zTOFCend    ,TPCG_LengthW/2},
                Rmn={TPCG_WheelR2*cos15 ,TPCG_WheelR2*cos15 ,TPCG_WheelR2*cos15,TPCG_WheelR2*cos15  },
                Rmx={TPCG_WheelOR1      ,TPCG_WheelOR1      ,TPCG_WheelOR      ,TPCG_WheelOR        }

      Attribute TWRI seen=0  colo=kGreen

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

      Create And Position TWRC

      myCos = cos15; myRmin=TPCG_WheelR2; myRmax = TPCG_WheelOR1;
      myG1 = myRmin*cos15;

      nCount = 0;

      DO WHILE ( myg2 .LT. myRmin ) 

         ncount += 1
         myg2 = myRmin

         IF ( myg2 .GT. myRMax*myCos ) myG2 = myRMax*myCos ;
         alpha = ACOS(myCos)/Pi*180

         Create and Position TWRG
         myCos = myG2/myRmin; myG1=myG2;

         IF (nCount .GT.10) break;
         
      END DO


endBlock        "end TWRI"


!=============================================================================
BLOCK TWRG WheelOuterRing PGON part
Attribute TWRG seen=1  colo=kGreen
MATERIAL ALUMINIUM
      SHAPE     PGON    Phi1=-alpha  Dphi=alpha*2  Nz=2 Npdv=1,
       zi ={zWheel1,TPCG_LengthW/2},
       Rmn={myG1   ,myG1          },
       Rmx={myG2   ,myG2          }

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TWRG"


!=============================================================================
BLOCK TWRC WheelOuterRing PCON part
Attribute TWRC seen=1  colo=kGreen
MATERIAL ALUMINIUM
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=4,
       zi ={zWheel1           ,zTOFCend          ,zTOFCend          ,TPCG_LengthW/2    },
           Rmn={TPCG_WheelR2/cos15,TPCG_WheelR2/cos15,TPCG_WheelR2/cos15,TPCG_WheelR2/cos15},
       Rmx={TPCG_WheelOR1     ,TPCG_WheelOR1     ,TPCG_WheelOR      ,TPCG_WheelOR      }

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TWRC"


!=============================================================================
BLOCK TWTR      TpcWheelTopRib

Attribute TWTR seen=1  colo=kBlue
MATERIAL ALUMINIUM
      myG1=TPCG_WheelR2 - (TPCG_WheelTotalRibWidth-TPCG_WheelRibWidth)/2;
      SHAPE  PGON    Phi1=-15  Dphi=30  Nz=2 Npdv=1,
       zi ={zWheel1     ,zWheel1+TPCG_WheelRibHeight},
           Rmn={myG1        ,myG1},
       Rmx={TPCG_WheelR2,TPCG_WheelR2}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TWTR"


!=============================================================================
BLOCK TWMR TpcWheelMiddleRib

Attribute TWMR seen=1  colo=kYellow
MATERIAL ALUMINIUM
      myG1=TPCG_WheelR2 - (TPCG_WheelTotalRibWidth-TPCG_WheelRibWidth)/2;
      SHAPE  PGON    Phi1=-15  Dphi=30  Nz=4 Npdv=1,
       zi ={zWheel1     ,zWheel1+TPCG_WheelRibHeight,zWheel1+TPCG_WheelRibHeight,TPCG_LengthW/2},
           Rmn={TPCG_WheelR1 - TPCG_WheelTotalRibWidth/2,
                TPCG_WheelR1 - TPCG_WheelTotalRibWidth/2,
                        TPCG_WheelR1 - TPCG_WheelRibWidth/2     ,
                        TPCG_WheelR1 - TPCG_WheelRibWidth/2     ,},
       Rmx={TPCG_WheelR1 + TPCG_WheelTotalRibWidth/2,
                TPCG_WheelR1 + TPCG_WheelTotalRibWidth/2,
                        TPCG_WheelR1 + TPCG_WheelRibWidth/2,
                        TPCG_WheelR1 + TPCG_WheelRibWidth/2}

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TWMR"


!=============================================================================
BLOCK TRDO  TpcRDOAssembly


     SHAPE  PGON    Phi1=-15  Dphi=30  Nz=2 Npdv=1,
        zi ={zzzRDO(1),zzzRDO(2)},
        Rmn={rmnRDO(1),rmnRDO(2)},
        Rmx={rmxRDO(1),rmxRDO(2)}
        Attribute TRDO  seen=0 
     Attribute TRDO seen=1  colo=kYellow

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
     
     mySha = 'PARA';
     myPar = {TPCG_RDOCoolingdY, TPCG_RDOCoolingdX, TPCG_RDOCoolingdZ, 15};
     x = TPCG_WheelR2 - TPCG_RDOCoolingdX;
     y = x*tan15 - TPCG_RDOCoolingdY ;
     z = TPCG_zRDO - 2*dy;
     Create And Position TCOO x=x y=y z=z ORT=YX-Z

     mySha = 'PARA';
     myPar = {TPCG_RDOCoolingdY, TPCG_RDOCoolingdX, TPCG_RDOCoolingdZ, -15};
     x = TPCG_WheelR2 - TPCG_RDOCoolingdX;
     y = x*tan15 - TPCG_RDOCoolingdY ;
     z = TPCG_zRDO - 2*dy;
     Create And Position TCOO x=x y=-y z=z ORT=YX-Z



     RCoolingTube = TPCG_WheelR2 - TPCG_heigTube - 2*TPCG_dxRDO;
     DO iCoo = 0,7

        dz = TPCG_heigTube*cm/2;
        dy = TPCG_widTube*cm/2;
        z  = RCoolingTube;
        dx1 = (z-dz)*tan15 - 2*TPCG_RDOCoolingdY;
        dx2 = (z+dz)*tan15 - 2*TPCG_RDOCoolingdY;

        mySha = 'TRD1';
        myPar = {dx1, dx2, dy, dz};
        myPar(11) = z; myPar(13) = TPCG_zRDO - 2*dy;
        Create And Position TCOO x=myPar(11) y=0 z=myPar(13) ORT=YZX


        IF (iCoo. ne. 0 .AND. iCoo .NE. 6) THEN

           myPar(11) = z + dz + TPCG_dxRDO; 
           myPar(13) =          TPCG_zRDO-TPCG_dzRDO;

           Create And Position TRDC x=myPar(11) y=0 z=myPar(13);

        ENDIF

        RCoolingTube -= dRDOCooling(iCoo);

     ENDDO

endBlock        "end TRDO"


!=============================================================================
BLOCK TBRW  WheelRibBox
   Attribute TBRW seen=1  colo=kViolet
   MATERIAL ALUMINIUM
   SHAPE  BOX dX=TPCG_WheelBoxDy/2 dY=TPCG_WheelBoxDx/2 dZ=TPCG_WheelTHK/2

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TBRW"


!=============================================================================
BLOCK TWRB TpcWheelRib
   Attribute TWRB seen=1  colo=kYellow
   MATERIAL ALUMINIUM
   SHAPE PARA dX=myDx dY=myDy dz=myDz Alph=myAlph

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

EndBlock        "end TWRB"


!=============================================================================
BLOCK TCOO      CoolingTube

   Attribute TCOO seen=1  colo=kRed
   MATERIAL Water_Pipe
   
   IF (mySha .EQ. 'PARA') THEN
      SHAPE PARA dX=myPar(1) dY=myPar(2) dz=myPar(3) Alph=myPar(4)
   ENDIF
   IF (mySha .EQ. 'TRD1') THEN
      SHAPE TRD1 dX1=myPar(1) dX2=myPar(2) dY=myPar(3) dZ=myPar(4)
   ENDIF

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TCOO"


!=============================================================================
BLOCK TCAB      Cables
   Attribute TCAB seen=1  colo=kGreen
   MATERIAL Cables
   SHAPE PARA dX=myPar(1) dY=myPar(2) dz=myPar(3) Alph=myPar(4)

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TCAB"



!=============================================================================
BLOCK TRDC      RDOCard
     Attribute TRDC seen=1  colo=kBlue
     MATERIAL G10
     SHAPE PARA dX=TPCG_dxRDO dY=TPCG_dyRDO dZ=TPCG_dzRDO

      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);

endBlock        "end TRDC"


!=============================================================================
BLOCK TRIB      Tpc Ribs

     Attribute TRib seen=1  colo=kRed
     MATERIAL ALUMINIUM

     IF     (mySha == 'PARA') THEN
        SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3) Alph=myPar(4)
     ELSEIF (mySha == 'TRD1') THEN
        SHAPE TRD1 dX1=myPar(1) dX2=myPar(2) dY=myPar(3) dZ=myPar(4)
     ELSEIF (mySha == 'BOX ') THEN
        SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3)
     ELSE
        Prin0 mySha;
          ('============================= ERROR in TRIB: Unknown shape:',A4);
        Prin0 %ivolume,%volume;
          ('Creating volume ',I4,2X,A4);
        Prin0 %imother,%mother;
          ('+ moth=',I4,2X,A4);     
        Prin0 %imat,%material;
          ('+ imat=',I4,2X,A20);
        Prin0 %imed,%medium;
          ('+ imed=',I4,2X,A20);
        Prin0 %shape;
          ('+ shape=',A4);
        Prin0 %parlist;
          ('+ list=',A20);
     ENDIF

     Prin1 %ivolume,%volume;
       ('Creating volume ',I4,2X,A4);
     
endBlock        "end TRIB"


!=============================================================================
BLOCK TWIR      WireMount

    Attribute TWIR seen=1  colo=kYellow
    MATERIAL G10
    SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3) Alph=myPar(4)
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
endBlock        "end TWIR"


!=============================================================================
BLOCK THOL the Sector holes for FEE

        Attribute THOL seen=1  colo=kGreen
        MATERIAL Air;
        SHAPE BOX dX=myPar(1) dY=myPar(2) dZ=myPar(3)
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
        Create and Position FEES

endBlock        "end THOL"


!=============================================================================
BLOCK FEES  TFEESocket
    MATERIAL  G10
    Attribute FEES      seen=1  colo=kBlue
    SHAPE     BOX    dX=6.2/20, dY=57./20, dZ=5.8/20
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
endBlock        "end FEES"


!=============================================================================
!*****************************************************************************
!*                      section one - sensitive gas                          *
!*****************************************************************************
!=============================================================================
BLOCK TPGV is the Gas Volume placed in TPC

    Attribute TPGV      seen=1  colo=kRed
    MATERIAL P10
    SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpgvLeng/2
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
    DO iSecAng = 15,360-15,30
       Create and Position TPSS            alphaz=iSecAng kOnly='MANY'
    ENDDO
Endblock


!=============================================================================
BLOCK  TPSS is a division of gas volume corresponding to a supersectors
      attribute TPSS  seen=1  colo=kBlue
!     SHAPE Division  NDIV=12  IAXIS=2
      SHAPE TUBS Phi1=-15 Phi2=15
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
      ag_ncopy = 1;
      DO kase=1,2
         DO i_sec=1,2
            USE    TPRS  sec=i_sec;
            USE    TECW  sec=i_sec;
            zWheel1  = TPCG_LengthW/2 - TPCG_WheelTHK
            zBeg = TPCG_MembTHK/2
            zEnd = TPCG_zGroundGrid + TPRS_dAnode*2;
            zDed = TPCG_zGatingGrid - TPCG_DeadZone;
            zPmt = TPCG_zGatingGrid;
            dx=tprs_width/2;

            IF (kase == 1) THEN
               dz = (zDed - zBeg)/2; z= (zDed + zBeg)/2 -tpgvz;
            ENDIF
            IF (kase == 2) THEN
               dz = (zEnd - zPmt)/2; z= (zEnd + zPmt)/2 -tpgvz;
            ENDIF

            !        position within supersector (this assumes rectangular padrows)
            DO i_row = 1,NINT(tprs_nRow)
               IF ((NINT(tprs_super)==3 | i_row==1)) THEN
                  dy=tprs_npads(i_row)*tprs_pitch/2;
                  x=tprs_Rpads(i_row)-tprs_width;
                  Create and Position TPAD  x=x z=z dx=dx dy=dy dz=dz

               ENDIF
               dy=tprs_npads(i_row)*tprs_pitch/2;
               x=tprs_Rpads(i_row);
               Create and Position TPAD  x=x z=z dx=dx dy=dy dz=dz

               IF ((NINT(tprs_super)==3 | i_row==NINT(tprs_nRow)))  THEN
                  x=tprs_Rpads(i_row)+tprs_width
                  dy=tprs_npads(i_row)*tprs_pitch/2;
                  Create and Position TPAD  x=x z=z dx=dx dy=dy dz=dz

               ENDIF
            ENDDO
         ENDDO
      ENDDO

Endblock


!=============================================================================
BLOCK TPAD is a REAL padrow WITH dimensions defined at positioning time
!*
!* We want to use PAI model in padrows to simulate energy loss more precisely.
!* (see GEANT PHYS334, Allisson and Cobb, Ann.Rev.Nucl.Part.Sci.30(1980),253).
!* (    best formalism in Grishin,Ermilova,Kotelnikov, NIM A307(1991),273).
!* To switch it ON only in this volume we introduce P10 as a different MATERIAL,
!* since ISTRA flag is kept in MEDIUM, but all tables are stored in the MATERIAL
!* According to AGI rules for this we need A parameter. Lets use ISVOL=1 (!)
!* (- this eliminates a need for a separate MEDIUM definition ?)
!*
      Attribute TPAD seen=1 colo=2
      MATERIAL p10
      MATERIAL sensitive_gas  ISVOL=1  stemax=2.5*tprs_width
      SHAPE    BOX   dx=0   dy=0   dz=0
      Prin1 %ivolume,%volume;
        ('Creating volume ',I4,2X,A4);
      CALL     GSTPAR(ag_imed,'CUTGAM',1e-5)
      CALL     GSTPAR(ag_imed,'CUTELE',1e-5)
      CALL     GSTPAR(ag_imed,'DCUTE',1e-5)
      CALL     GSTPAR(ag_imed,'DCUTM',1e-5)
*      CALL     GSTPAR(ag_imed,'STRA',1.)
!
!     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    TPAD   Z:.0005:S  Y:.0005:  X:.0005:   cx:10: cy:10: cz:10:,
                     LPtot:18:(-3,2)      Sleng:.1:(0,800),
                     ToF:16:(0,1.e-6)     LGAM:16:(-4,6),
                     Step:11:(0,10)       USER:21:(-.01,.01)
EndBlock
!=============================================================================

      END
