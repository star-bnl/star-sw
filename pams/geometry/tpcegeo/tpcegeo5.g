!// $Id: tpcegeo4.g,v 1.28 2011/02/24 15:48:20 jwebb Exp $
!// $Log: tpcegeo4.g,v $
!// Revision 1.28  2011/02/24 15:48:20  jwebb
!// Volumes TBRW (Aluminum) and TWAS (Air) overlap.  Changed positioning of
!// TBRW so that it is positioned as an ONLY volume, to remove ambiguity in
!// geant tracking.  The change has no effect on an AgSTAR geometry, see
!//
!// http://drupal.star.bnl.gov/STAR/node/20519
!//
!// Revision 1.27  2010/12/17 20:02:11  jwebb
!//
!// Reverted max radius to previous value.  Reduced radius will be set by
!// TPCE04r flag in geometry.g.
!//
!// Revision 1.26  2010/09/19 19:39:01  jwebb
!// Reduced size of TPC envelope to acommodate TOF.
!//
!// Revision 1.25  2010/08/20 20:32:15  jwebb
!// Increased size of array to prevent an out-of-bounds condition.
!//
!// Revision 1.24  2010/01/13 22:36:24  perev
!// Typo is fixed. TPCG was twice as bigger
!//
!// Revision 1.23  2009/12/30 19:10:28  perev
!// Wrong rotation(15) of TPSS fixed
!//
!// Revision 1.22  2009/12/24 17:36:46  perev
!// Gas volume 210
!//
!// Revision 1.21  2009/12/23 21:35:35  perev
!//  TSAS zmax increased by 0.1
!//
!// Revision 1.20  2009/12/17 23:07:11  perev
!// More exact envelopes for TPCE,TSAW,TSWH
!//
!// Revision 1.19  2009/12/15 02:44:07  perev
!// TPCE increased in R & Z to avoid extruding
!//
!// Revision 1.18  2009/12/12 03:38:37  perev
!// extrudedFix
!//
!// Revision 1.17  2009/11/10 23:04:42  perev
!// remove debug prints
!//
!// Revision 1.16  2009/11/10 02:14:31  perev
!// Where GSTPAR, set local material avoid bug in gphysi
!//
!// Revision 1.15  2009/10/30 18:40:57  perev
!// Some redundant prints removed
!//
!// Revision 1.14  2009/10/28 23:59:28  perev
!// Assembly volumes seen=0
!//
!// Revision 1.13  2009/10/27 20:07:41  fisyak
!// Reduce cuts from 1 MeV to 100 keV
!//
!// Revision 1.12  2009/10/27 00:33:31  perev
!// All assemblies must be MANY
!//
!// Revision 1.11  2009/10/24 21:41:55  perev
!// Prompt hits from GateGrid==>PadPlane
!//
!// Revision 1.10  2009/09/27 23:52:33  perev
!// 1st (wrong) version of prompt hits
!//
!// Revision 1.9  2009/09/01 19:43:16  perev
!// Fix FEEA position bug #1570
!//
!// Revision 1.8  2009/08/28 16:50:12  perev
!// CleanUp of write(*,*)
!//
!// Revision 1.7  2009/08/25 19:48:47  perev
!// wrong TPAD shift fixed
!//
!// Revision 1.6  2009/07/25 02:07:56  perev
!// Prompt hits added
!//
!// Revision 1.5  2009/03/01 18:46:36  perev
!// double shift -15 fixed
!//
!// Revision 1.4  2009/02/24 19:50:08  perev
!// TSWH MANY temporary
!//
!// Revision 1.3  2009/02/24 19:38:06  perev
!// gstar bug workaround
!//
!// Revision 1.2  2009/02/23 00:05:35  perev
!// remove qfatal call
!//
!// Revision 1.1  2009/02/22 21:39:58  perev
!// Y2009 born
!//
!// Revision 1.11  2008/12/01 21:51:52  fisyak
!// Add Cables to Tpc, add more indexes
!//
!// Revision 1.10  2008/11/25 00:07:44  fisyak
!// use new Tpc Geometry
!//
!// Revision 1.5  2008/11/25 00:06:37  fisyak
!// Fix overlaps
!//
!// Revision 1.4  2008/11/20 17:21:02  fisyak
!// Clean up
!//
!// Revision 1.3  2008/11/20 00:10:39  fisyak
!// Add Wire Mount
!//
!// Revision 1.2  2008/11/18 00:06:12  fisyak
!// Finish with Wheel
!//
!// Revision 1.1  2008/11/17 14:14:46  fisyak
!// new Tpc Geometry
!//
!// Revision 1.8  2008/10/21 14:57:29  fisyak
!// add missing gaps in wheel
!//
!// Revision 1.7  2008/10/20 16:14:37  fisyak
!// Clean up
!//
!// Revision 1.6  2008/10/20 15:40:59  fisyak
!// Freeze TIFC and TOFC
!//
!// Revision 1.5  2008/09/03 20:44:48  fisyak
!// replace tpc geometry with translate one from mortran, clean ups
!//
!// Revision 1.4  2008/08/27 21:48:17  fisyak
!//
!// TPC
 Replace [;ASSERT(#);] with [;call assert((#1),__LINE__,'TPCE');];
 module   TPCEGEO5  is the updated TPC
   Author   David Underwood corrected by  Maxim Potekhin
   Created  Dec 7, 2005
*  Found incosistencies in the hx array initialization
*  Original Version - Peter Jacobs (LBL), March 16, 1995.                   *
*  Please see the file tpcegeo.g for the original comments for the previous *
*  version of the code, which has since been enriched with new content      *
*                                                                           *
*  DGU,HMS  15/02/05:  Aded FEE Cards, Holes for FEE cards, Water Pipes for *
*               FEE CArds, Water Manifolds, Cutouts and Blocks in Wheels,   *
*               and  RDO boards in addtiional +- 30 cm length of tpcegeo    *
*****************************************************************************
+cde,AGECOM,GCUNIT,GCONST.     " - standard geant commons "
character *4 voluName;
Content   TPCE,TOFC,TOFS,TOST,TOKA,TONX,TOAD,TOHA,TPGV,TPSS,
          TIFC,TIAL,TIKA,TINX,TPCW,TWSS,TWGI,TPCM,TPEA,
          TESS,TSWH,TMWC,TMEA,TMSE,TIAG,TOAE,TPAD,TPAI,TPAO,TDEA,
          THOL,THRA,THLA,TALC,TAEC,TCEX,TCRX,TSAW,
          TWGC,TWGB,TPIP,TMAN,TRDV,TRDS,TRDC,TIAD,TOIG,
          FEES,FEEP,FEER,FEEI,FEEA,
          TSAS,TWAS,TALS,TSGT,TWBT,TWRC,TWRG,TWRI,TWTR,TWMR,
          TRDO,TBRW,TWRB,TCOO,TCAB,TRIB,TWIR
*
        Real INCH ,CM,sind,cosd,tand,cos15,sin15,tan15;
        Parameter (INCH=2.54,CM=1.);
        integer kBlack,kRed,kGreen,kBlue,kYellow,kViolet,kLightBlue;
        parameter (kBlack=1,kRed=2,kGreen=3,kBlue=4);
        parameter (kYellow=5,kViolet=6,kLightBlue=7);

!//  Real lb   = 0.45359; !// kG
        parameter (cos15 =.965925826289068312) !// Cos(15./180*Pi);
        parameter (sin15 =.258819045102520739) !// Sin(15./180*Pi);
        parameter (tan15 =.267949192431122696) !// Tan(Pi*15./180);
!//   int's
   Integer i,j,iSecAng,nCount/0/,jTRIB
!//   real's
  Real x,x0,x1,x2,dx,dx1,dx2,xc;
  Real y,y0,y1,y2,dy,dy1,dy2;
  Real z,z0,z1,z2,dz,dz1,dz2;
  Real zBeg,zDed,zPmt,zEnd;
  Real r,r0,r1,r2,dr;
  Real alpha,beta;
  Real xw,dxw,yw,dyw,zw,dzw,dYdX
  Real dRSectorShift/0/
  Real myDx,myDy,myDz,myAlph,myThet,myPhi,myPar(20)

!// radii of outer finest structureures
        Real tocsIR,tocsOR,tokaIR,tokaOR,tonxIR ,tonxOR ,toadIR,toadOR,toigIR,toigOR;
        Real toalIR,toalOR,tohaIR,tohaOR,toalIR2,toalOR2,tofcIR,tofcOR,tofsIR,tofsOR;

!// radii of inner finest structureures
        Real tifcIR,tialIR,tialOR,tikaIR,tikaOR,tinxIR,tinxOR,tiadIR,tiadOR,tifcOR;

!//   variables from tpcegeo2
        Real tpgvLeng,tofcLeng,tpgvz;
        Integer i_sec,i_row;
External  TPADSTEP,TPAISTEP,TPAOSTEP,TPCELASER

!//  derive radii of larger structureures
        Real    tpgvIR; !// TPC gas inner radius

!//  make primitive control and basic printout
        Real del;

   Real zWheel1,zTOFCend ,zTIFCend,zTIFCFlangeBegin;
   Real qwe(10);


!//
        integer kXXrib,kDXrib,kYYrib,kDYrib;
        Parameter (kXXrib=0,kDXrib=1,kYYrib=2,kDYrib=3);
        Real    RIB(0:3,0:15,0:1),RIBI(4,16),RIBO(4,16);
        Equivalence (RIB(0,0,0),RIBI),(RIB(0,0,1),RIBO);
        integer noHolesRows(0:1) /15, 16/;
        integer noHolesPerRow(0:15,0:1),noHolesPerRowI(0:15),noHolesPerRowO(0:15);
        equivalence     (noHolesPerRow(0,0),noHolesPerRowI(0)),
                        (noHolesPerRow(0,1),noHolesPerRowO(0));
        data noHolesPerRowI /6, 5, 5, 2, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 0/;
        data noHolesPerRowO /9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 6/;

        Real yhI(0:160),yhO(0:160),yhOF(160),yHoles(0:160,0:1);
        equivalence     (yHoles(0,0),yhI),(yHoles(0,1),yhO),(yhO,yhOF);

        Real xHoles(0:15);


!// Inner/Outer         {   24A3696A,     24A4296A} Sector Cooling Manifold Overall Dimensions
  Real width1IO(0:1), width2IO(0:1),heightIO(0:1),depthIO(0:1)
  Real width1IOF(2) , width2IOF(2) ,heightIOF(2) ,depthIOF(2)
  Equivalence (width1IO,width1IOF),(width2IO,width2IOF)
  Equivalence (heightIO,heightIOF),(depthIO ,depthIOF )

  Real InnerCoolingTubeLength(0:15)   / 19.300, 18.454, 17.610, 16.766, 15.922,
                                        15.078, 14.235, 13.390, 12.546, 11.704,
                                        10.860, 10.016,  9.172,  8.327,  7.483, 0/;
!// 15 degrees upper edge
  Real OuterCoolingTubeLength(0:15)   / 33.620, 33.605, 32.761, 31.917,
                                        31.073, 30.229, 29.385, 28.541,
                                        27.697, 26.853, 26.009, 25.165,
                                        24.321, 23.478, 22.634, 21.790/;
  !// FEE + Cooling manifold Assembly Drawings 24A0882A
  !// Cooling Drawing 24A4096C

  Real CoolingTubeLength(0:15,0:1);
  Equivalence (CoolingTubeLength(0,0),InnerCoolingTubeLength(0));
  Equivalence (CoolingTubeLength(0,1),OuterCoolingTubeLength(0));

!//     Inner/Outer index
  Integer inOut;

!//     Tpc(Inner/Outer)SectorAssembly
  Real zzzSA(2)         /207.4   ,219.1 /
  Real rmnSA(0:1)       /51      ,121.07/
  Real rmxSA(0:1)       /121.07  ,194   /

!//     Tpc(Inner/Outer)WheelAssembly
  Real zzzWA(2)         /211.4,229.2/
  Real rmnWA(0:1)       /54   ,121  /
  Real rmxWA(0:1)       /121  ,189  /

!//             TpcRDOAssembly
  Real zzzRDO(2)/232. ,252./;
  Real rmnRDO(2)/ 88. , 88./;
  Real rmxRDO(2)/198. ,198./;


!//             OuterRing
  Real myCos,myRmin,myRmax,myG1,myG2;

!// RDO & cool
  Real dRDOCooling(0:7) /16.0, 16.0, 16.0, 16.7, 15.0, 3.5, 14.5,0.0/;
  Real RCoolingTube/123456789/;
  character *4 mySha;
  integer iCoo,iCab,jCoo,iRib,iTALS,jTALS;






!//             TPC Parameters
    structure TPCG {    version,TpadConfig,Rmin,Rmax,RminIFC,
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
!//  YF temporary off
!//YF  structure TPRS {sec, nRow, pitch, width,  dAnode, Npads(50), Rpads(50)}
Structure TPRS { sec,Nrow,pitch,width,super,dAnode,Rpads(50),Npads(50) }

!// EC trapezoid and support Wheel
 structure TECW {sec, GapWidI, GapWidO, GapHeit, GapRad, inwidth, ouwidth,
                 widthLip, noribs, zStepRib, widthRib, Height, Thick,
                 ThickAl, rMin, Rcenter,holeDx,holeDy}

Structure TPCR { RdoVthk,Rdothk,Rdolen,NRdobrd,Rdoht(9) }

!//     FEE stuff
Structure TFEE {Vers,CardDX ,CardDY,CardDZ,PlateDX,PlateDY,PlateDZ,
                AssemblyThickness,RibDX ,RibDY  ,RibDZ, Ass(3) }


  Fill  TPCG !//   TPC basic dimensions
        version =       5               !// version    => current version
	TpadConfig=     0               !// default
        Rmin =          46.107          !// Rmin          => TPC envelope inner radius
        Rmax =          206.75    ! TPC outer envelope 
        RminIFC =       46.6            !// RminIFC    => inner radius TPC IFC  : A.Lebedev measurement 10/16/08
        LengthT =       2*271.0         !// LengthT    => TPC full length up to front of EEMC
        Length  =       2*259.685       !// Length        => TPC full length including RDOs
        LengthW =       2*229.71        !// LengthW    => TPC length including Wheel
        LengthV =       2*210.107       !// LengthV    => TPC (Outer) gas volume length 
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
        dxRDO   = 1.75/2                !// A.Lebedev 1 RDO = 5 lb  10/24/14 A.Lebedev  945g
        dyRDO   = 45.72/2               !//
        dzRDO   = 17.78/2               !//
        zRDO    = TPCG_LengthW/2 + 20.0         !//
        heigTube = 0.703*INCH           !// x Cooling TUBE Drawing 24A0801B
        widTube  = 0.500*INCH           !// z Mixture TPCE_Water_Pipe => rho = 2.32155 g/cm**3
        RDOCoolingdX = (38.0 + 9.0 + 58.0)/2    !//
        RDOCoolingdY = 1.25                     !//
        RDOCoolingdZ = 2.50                     !//
        dAnode = { 0.2 , 0.4 }                  !// Inner/Outer anode width
        zGatingGrid = 208.707 !TPCG_LengthV/2 -(TPCG_dGateGround+2*TPCG_dAnode(2)) 	!// 
        zGroundGrid = TPCG_zGatingGrid+TPCG_dGateGround       		!//
        DeadZone      = 14. !// Dead zone before GatingGrid. No hits there
        endFill

        USE TPCG
	if (TPCG_TpadConfig == 0) {
   Fill TPRS                    ! sector of padrows
      sec    = 1                ! sector number: 1 for inner, 2 for outer
      nRow   = 13               ! number of padrows in the sector
      pitch  = 0.335            ! tpc padrow pitch width
      width  = 1.15             ! tpc padrow thickness
      super  = 3                ! number of padraws in a superpadrow
      dAnode = 0.2              !// distance to anode wire from pad plane
      Npads  = { 88, 96, 104, 112, 118, 126, 134, 142, 150,
                158, 166, 174, 182 }        ! number of pads in row
      Rpads  = {60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8,
               104.0,109.2,114.4,119.6 }    ! tpc padrow radii
   EndFill
        }	
	else { ! if (TPCG_TpadConfig == 8) {
   Fill TPRS                    ! devTY: sector of padrows  
      sec    = 1                ! sector number: 1 for inner, 2 for outer  
      nRow   = 40               ! number of padrows in the sector  
      pitch  = 0.5              ! tpc padrow pitch width  
      width  = 1.6              ! tpc padrow (pitch) thickness 
      super  = 0                ! number of padraws in a superpadrow  
      dAnode = 0.2              ! distance to anode wire from pad plane
      Npads  = {     52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
                     70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
                     86, 88, 90, 92, 94, 96, 98, 98,100,102,
                    104,106,108,110,110,112,114,116,118,120} ! number of pads in row, J.Thomas : 05/31/2016
      Rpads  = {55.8,  57.4,    59,  60.6,  62.2,  63.8,  65.4,    67,  68.6,  70.2,
                71.8,  73.4,    75,  76.6,  78.2,  79.8,  81.4,    83,  84.6,  86.2,
                87.8,  89.4,    91,  92.6,  94.2,  95.8,  97.4,    99, 100.6, 102.2,
               103.8, 105.4,   107, 108.6, 110.2, 111.8, 113.4,   115, 116.6, 118.2} ! tpc padrow radii
   EndFill
        }
*
   Fill TPRS                    ! sector of padrows
      sec    = 2                ! sector number: 1 for inner, 2 for outer
      nRow   = 32               ! number of padrows in the sector
      nRow   = 32               ! number of padrows in outer sector
      pitch  = 0.67             ! outer tpc padrow pitch width
      width  = 1.95             ! outer tpc padrow thickness
      super  = 1                ! number of padrows in a superpadrow
      dAnode = 0.4              !// distance to anode wire from pad plane
      Npads  = { 98, 100, 102, 104, 106, 106, 108, 110, 112,
                112, 114, 116, 118, 120, 122, 122, 124, 126,
                128, 128, 130, 132, 134, 136, 138, 138, 140,
                142, 144, 144, 144, 144 } ! number of pads in row
      Rpads  = {127.195, 129.195, 131.195, 133.195, 135.195,
                137.195, 139.195, 141.195, 143.195, 145.195,
                147.195, 149.195, 151.195, 153.195, 155.195,
                157.195, 159.195, 161.195, 163.195, 165.195,
                167.195, 169.195, 171.195, 173.195, 175.195,
                177.195, 179.195, 181.195, 183.195, 185.195,
                187.195, 189.195 }        ! tpc padrow radii
   EndFill
*
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
   widthRib     =         1.47*Cos(15./180*acos(-1.))*INCH !// widthRib => side rib width
   Height       =         27.373*INCH   !// Drawing 24A3685B 27.77*INCH from drawing 24A0325,
                                        !// => 69.527 => (Wire Plane)  69.52,  Height   => sector radial Height
   Thick        =         3.300*INCH    !// 24A3685B  3.219*INCH        !// Thick    => sector thickness
   thickAl      =         0.375*INCH    !// " 9.525mm ThickAl  => Thick - air gap"
   rMin         =         51.905        !// rMin     => Minimum distance to beam line (wire plane)
   Rcenter      =         86.669        !// Rcenter  => sector center radius (set by precision holes)
   holeDx       =         0.750/2*INCH  !// = 0.9525 cm
   holeDy       =         2.625/2*inch  !// = 3.33375 cm
   endFill

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
   widthRib     =         1.47*Cos(15./180*acos(-1.))*INCH      !// widthRib = > side rib width
   Height       =         28.155*INCH   !// 71.514 (Wire Plane) => 71.51,       !// Height   => sector radial Height
   Thick        =         3.140*INCH    !// 24A3925G thick    => sector thickness
   ThickAl      =         0.375*INCH    !// "= 9.525 mm; ThickAl  => Thick - air gap"
   rMin         =         121.732       !// rMin     => Minimum distance to beam line (wire plane)
   Rcenter      =         157.488       !// Rcenter  => sector center radius (set by precision holes)
   holeDx       =         0.750/2*INCH  !//
   holeDy       =         2.500/2*INCH  !//
endFill
*
Fill TPCR              ! volume for tpc readout boards
   RdoVthk   = 30.     ! length of RDO volume
   Rdothk    =.25      ! thickness of rdo card
   Rdolen    = 27      ! card length along beam direction
   NRdobrd   = 9       ! number of RDO boards
   Rdoht = {60.0, 74.0, 84.0, 101.0,106.0,
            126.0,146.0,166.0,186.0} ! radial pos of rdo boards
EndFill
*
Fill TFEE                               ! frontEndElectronics
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
USE TFEE

USE TECW
      Use TPCR
      Use TPCG
!//  /*
!//   *  TPC basic dimensions are the full system size, the gas volume length
!//   *  and inner radius are derived from them and from material thicknesses.
!//   *  The outer gas radius is also an input ( as this is used for winding ),
!//   *  but the remaining clearance is checked to be positive (PN, 16 Mar 96).
!//   */
!//  /*
!//   * layer names mnemonic:
!//   *-----------------------------------------------------------------------
!//   * letter 1-6 :   t  :     i/o     :  cs/ka/nx/al/fc/fs  :     IR/OR    :
!//   *-----------------------------------------------------------------------
!//   *   meaning  :  TPC : inner/outer :    materials as     :  inner/outer :
!//   *            :      :     cage    : Copper shild,Kapton :    radius    :
!//   *            :      :             : Nomex,Aluminum  OR  :    of the    :
!//   *            :      :             : field cage and      :    layer     :
!//   *            :      :             : field cage support  :              :
!//   *-----------------------------------------------------------------------
!//   */

 tofcLENG = tpcg_Length-2*tpcg_WheelTHK-2*TPCR_RdoVthk  !// gas plus endcaps
!//? tpgvLeng = (tofcLeng-tpcg_MembTHK)/2   	!// active gas
 tpgvLeng = (TPCG_LengthV-tpcg_MembTHK)/2 	!// active gas


!// calculate radii of outer finest structureures
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

 !// ?

!//   write(*,*)  'tocsIR = ' , tocsIR , '  tocsOR = ' , tocsOR ;
!//   write(*,*)  'tokaIR = ' , tokaIR , '  tokaOR = ' , tokaOR ;
!//   write(*,*)  'tonxIR = ' , tonxIR , '  tonxOR = ' , tonxOR ;
!//   write(*,*)  'toadIR = ' , toadIR , '  toadOR = ' , toadOR ;
!//   write(*,*)  'toigIR = ' , toigIR , '  toigOR = ' , toigOR ;
!//   write(*,*)  'toalIR = ' , toalIR , '  toalOR = ' , toalOR ;
!//   write(*,*)  'tohaIR = ' , tohaIR , '  tohaOR = ' , tohaOR ;
!//   write(*,*)  'toalIR2= ' , toalIR2, '  toalOR2 =' , toalOR2;
!//   write(*,*)  'tofcIR = ' , tofcIR , '  tofcOR = ' , tofcOR ;
!//   write(*,*)  'tofsIR = ' , tofsIR , '  tofsOR = ' , tofsOR ;

  !// calculate radii of inner finest structureures
 tifcIR = TPCG_RminIFC;
 tialIR = tifcIR; tialOR = tialIR + TPCG_tialDR/2.;
 tikaIR = tialOR; tikaOR = tikaIR + TPCG_tikaDR;
 tinxIR = tikaOR; tinxOR = tinxIR + TPCG_tinxDR;
 tiadIR = tinxOR; tiadOR = tiadIR + TPCG_tiadDR;
 tifcOR = tiadOR + TPCG_tialDR/2.;

!// calculate radii of inner finest structureures
!//  write(*,*) 'tialDR = ' , TPCG_tialDR , '  tialIR = ' , tialIR , '  tialOR = ' , tialOR ;
!//  write(*,*) 'tikaDR = ' , TPCG_tikaDR , '  tikaIR = ' , tikaIR , '  tikaOR = ' , tikaOR ;
!//  write(*,*) 'tinxDR = ' , TPCG_tinxDR , '  tinxIR = ' , tinxIR , '  tinxOR = ' , tinxOR ;
!//  write(*,*) 'tiadDR = ' , TPCG_tiadDR , '  tiadIR = ' , tiadIR , '  tiadOR = ' , tiadOR ;
!//  write(*,*) 'tifcIR = ' , tifcIR      , '  tifcOR = ' , tifcOR ;
!//  derive radii of larger structureures
  tpgvIR = tifcOR;         !// TPC gas inner radius

  !//  make primitive control and basic printout
  del = TPCG_Rmax-tofcOR;
  write(*,*) ' TPCEgeo : tpcConfig = ',TPCG_version, ' TpadConfig :', TPCG_TpadConfig
  write(*,*) ' TPCEgeo : tpcg_rmax = ',TPCG_rmax
  write(*,*) ' TPCEgeo : maximum  TPC  Radius is ',tofcOR,' clearance is ',del;

  if (del<0) write(*,*)' *** TPCEgeo ERROR : outer clearance negative '  ,del;
!//   write(*,*) ' TPCEgeo: senset. gas inner radius ',tpgvIR;
!//   write(*,*) ' TPCEgeo: sensitive gas length is ', tpgvLeng;
!//  write(*,*) '*** Building the Underwood version of the TPC ***';

***  /*
***    tocsIR = 200            tocsOR = 200.013
***    tokaIR = 200.013        tokaOR = 200.028
***    tonxIR = 200.028        tonxOR = 201.028
***    toadIR = 201.028        toadOR = 201.048
***    toigIR = 201.048        toigOR = 206.748
***    toalIR = 206.748        toalOR = 207.148
***    tohaIR = 207.148        tohaOR = 207.748
***    tifcIR = 46.107
***    tiadIR = 46.107         tiadOR = 46.187
***    tinxIR = 46.187      tinxOR = 47.457
***    tikaIR = 47.457      tikaOR = 47.472
***    tialIR = 47.472      tialOR = 47.476
***    tofcIR = 200         tofcOR = 207.748
***    tofsIR = 200         tofsOR = 201.048
***                            tifcOR = 47.476
***    tpgvIR = 47.476  */

  width1IOF = { 7.661*inch, 22.287*inch};
  width2IOF = {20.552*inch, 35.422*inch};
  heightIOF = {24.222*inch, 24.840*inch};
  depthIOF  = { 1.800*inch,  1.800*inch};



!//  derive radii of larger structureures
!//  write(*,*)  'tifcOR = ' , tifcOR ;
!//  write(*,*)  'tpgvIR = ' , tpgvIR ;




!//   nomex - PN  8/3/96 following I.Sacreda note
      component C      A=12  Z=6  W=5
      component H      A=1   Z=1  W=8
      component O      A=16  Z=8  W=2
      mixture   Nomex  Dens=0.064

!//     mix = new TGeoMixture("TPCE_Adhesive",3,   1.2); // Mylar with density = 1.3 g/cm**3
!//     mix->DefineElement(0,12,6,0.625);
!//     mix->DefineElement(1,1,1,0.4166667E-01);
!//   mix->DefineElement(2,16,8,0.3333333);
      Component C       A=12    Z=6     W=0.625
      Component H       A=1     Z=1     W=0.4166667E-01
      Component O       A=16    Z=8     W=0.3333333
      mixture   Adhesive    dens=1.2

!//mix = new TGeoMixture("TPCE_AL_HONEYCOMB",3,  0.282000    );mix->SetUniqueID(++imat);
!//    mix->DefineElement(0,27,13,0.1050000E-01);
!//    mix->DefineElement(1,14,7,0.7395);
!//    mix->DefineElement(2,9,4.5,0.25);

      Component Al        A=27  Z=13   W=0.0105
      Component N         A=14  Z=7    W=0.7395
      Component Adhesive  A=9   Z=4.5  W=0.2500
      mixture   Al_honeycomb    dens=0.282

*   G10 is given as 60% SiO2 and 40% epoxy in ftpcgeo.g, from which
*   the following is taken
      Component  Si  A=28.08  Z=14  W=0.6*1*28./60.
      Component  O   A=16     Z=8   W=0.6*2*16./60.
      Component  C   A=12     Z=6   W=0.4*8*12./174.
      Component  H   A=1      Z=1   W=0.4*14*1./174.
      Component  O   A=16     Z=8   W=0.4*4*16./174.
      Mixture G10  Dens=1.7
      material  G10

!//mix = new TGeoMixture("TPCE_Water_Pipe",3,  2.32155);mix->SetUniqueID(++imat);
!//    mix->DefineElement(0,    1, 1,1.06546299984001044e-02);
!//    mix->DefineElement(1,   16, 8,8.52370399872008355e-02);
!//    mix->DefineElement(2,26.98,13,9.04108330014399053e-01);
      Component  H  A=1  Z=1  W=1.06546299984001044e-02
      Component  O  A=16 Z=8  W=8.52370399872008355e-02
      Component  AL A=27 Z=14 W=9.04108330014399053e-01
      Mixture Water_Pipe  Dens=2.32155
      material  Water_Pipe

!// mix = new TGeoMixture("Cables",4,  2.68);
!// mix->DefineElement(0, 63.54,29, 0.586); // Copper
!// mix->DefineElement(1, 12.01, 6, 0.259); // Carbon
!// mix->DefineElement(2,15.999, 8, 0.138); // Oxigen
!// mix->DefineElement(3,1.00794,1, 0.017); // Hydrogen
      Component  CU  A=63.54   Z=29  W=0.586            "// Copper"
      Component  C   A=12.01   Z=6   W=0.259            "// Carbon"
      Component  O   A=15.999  Z=8   W=0.138            "// Oxigen"
      Component  H   A=1.00794 Z=1   W=0.017            "// Hydrogen"
      Mixture Cables  Dens=2.68
      material  Cables

!//mix = new TGeoMixture("TPCE_P10",3,  0.154053E-02);
!//    mix->DefineElement(0,40,18,0.9574468);
!//    mix->DefineElement(1,12,6,0.3191489E-01);
!//    mix->DefineElement(2,1,1,0.1063830E-01);
*     TPC default gas P10: Ar/methane 9:1 by volume
      Component Ar    A=40  Z=18 W=9
      Component C     A=12  Z=6  W=1
      Component H     A=1   Z=1  W=4
      Mixture P10  Dens=0.154053E-02


!//__________________________________________ Geomety ______________________________________
*
*
!//      write(*,*) '*** Building the Underwood version of the TPC ***'
      create and position TPCE in CAVE
*
*------------------------------------------------------------------------------
*
Block TPCE is the TPC envelope
*
      material  Air
      Medium    Standard
      Attribute TPCE  seen=0 colo=kRed
!//   shape     TUBE  rmin=tpcg_rmin  rmax=tpcg_rmax  dz=tpcg_lengthT/2
      shape     TUBE  rmin=tpcg_rmin  rmax=tpcg_rmax  dz=tpcg_DzEnvelop

      tpgvz  = (tpcg_MembTHK + tpgvLeng)/2               " z center of gas volume   "
      zWheel1  = TPCG_LengthW/2 - TPCG_WheelTHK;        !// write(*,*) 'zWheel1 ', zWheel1;
      zTOFCend = TPCG_LengthW/2 - TPCG_WheelTHK1;       !// write(*,*) 'zTOFCend', zTOFCend; !// end of TOFC
      zTIFCend = TPCG_LengthW/2 + 0.97;                 !// write(*,*) 'zTIFCend', zTIFCend; !// end of TIFC + flange
      zTIFCFlangeBegin = zTIFCend - 1*INCH;             !// write(*,*) 'zTIFCFlangeBegin' , zTIFCFlangeBegin;

      Create and position  TPGV kONLY = 'MANY',
                                z=+tpgvz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TPGV kONLY = 'MANY',
                                z=-tpgvz,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0
*                                                                2

      Create and position  TPCM       "    membrane    "

      Create and position TIFC        "   inner cage   "
      Create and position TOFC        "   outer cage   "

     do iSecAng = 0,360-30,30
       Create and Position TSWH            alphaz=iSecAng kOnly='MANY'
       Create and Position TSWH ort = XY-Z alphaz=iSecAng kOnly='MANY'
     endDo

endBlock !// end TPCE
*
*******************************************************************************
*
Block TPCM is the Central Membrane placed in TPC
      material  Mylar
      Attribute TPCM  seen=1    colo=kBlue
*     Medium    dense_membrane  SteMax=1  " just to show that we define it "
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpcg_MembTHK/2
endBlock  "end TPCM"
*
*******************************************************************************
*
Block TIFC  defines the Inner Field Cage placed in TPC
*** Contents of Inner Field Cage structureure TIFC: Adhesive,Nomex,Kapton,Aluminum
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
**?   Material   MYLAR (changed by YF)
      Material   ALUMINIUM
      Attribute TIFC   seen=1  colo=kBlue
      SHAPE     PCON    Phi1=0  Dphi=360  Nz=6,
      zi ={-zTIFCend,-zTIFCFlangeBegin,-zTIFCFlangeBegin, zTIFCFlangeBegin,zTIFCFlangeBegin,zTIFCend},
      Rmn={tifcIR,tifcIR,tifcIR,tifcIR,tifcIR,tifcIR},
      Rmx={TPCG_tifcRF,TPCG_tifcRF,tifcOR,tifcOR,TPCG_tifcRF,TPCG_tifcRF}

      Create and position TIKA
      Create and position TINX
      Create and position TIAD
endBlock        "end TIFC"

*
*******************************************************************************
*
Block TIKA is the kapton film of the inner field cage
      Material  Mylar
      Attribute TIKA   seen=1  colo=kViolet
      SHAPE     TUBE   rmin=tikaIR rmax=tikaOR dz=TPCG_dzYF1
endBlock        "end TIKA"
*
*******************************************************************************
*
Block TINX is the inner nomex structureure
*
      Material  Nomex
      Attribute TINX   seen=1  colo=kYellow
      Shape     TUBE   rmin=tinxIR rmax=tinxOR dz=TPCG_dzYF1
endBlock        "end TINX"
*
*******************************************************************************
*
Block TIAD the inner adhesive structureure
      Material  ADHESIVE
      Attribute TIKA   seen=1  colo=kViolet
      SHAPE     TUBE   rmin=tiadIR rmax=tiadOR dz=TPCG_dzYF1
endBlock        "end TIAD"

*
*******************************************************************************
*
Block TOFC  defines outer field cage - fill it with insulating gas already
      material  ALUMINIUM
      Attribute TOFC   seen=1 colo=kGreen
      SHAPE     PCON   Phi1=0  Dphi=360  Nz=6,
      zi =  {-zTOFCend, -zWheel1,-zWheel1,zWheel1,zWheel1,zTOFCend},
      Rmn = {TPCG_WheelOR1,TPCG_WheelOR1,tofcIR,tofcIR,TPCG_WheelOR1,TPCG_WheelOR1},
      Rmx = {TPCG_WheelOR ,TPCG_WheelOR ,tofcOR,tofcOR,TPCG_WheelOR ,TPCG_WheelOR }
      create and position TOAD

      create and position TOFS
      create and position TOKA
      create and position TONX
      create and position TOIG
      create and position TOHA
endBlock        "end TOFC"
*------------------------------------------------------------------------------
*
*** Contents of Outer Field Cage structureure TOFS:  Copper/Kapton/Nomex/Adhesive
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block TOFS  is the Outer Field Cage structureure
!//   dz = 211.9;
!//   TGeoVolume *TOFS = gGeoManager->MakeTube("TOFS",GetMed("TPCE_COPPER"),tocsIR,tocsOR,TPCG_dzYF2);
!//   TOFS->SetTitle("the Outer Field Cage copper structureure");
!//   TOFS->SetLineColor(kBlue);
!//  //  TOFS->SetVisibility(1);
!//  TOFC->AddNode(TOFS,1,gGeoIdentity);
      material  COPPER
      Attribute TOFS      seen=1  colo=kBlue
      SHAPE     TUBE      rmin=tocsIR rmax=tocsOR dz=TPCG_dzYF2
*
endBlock        "end TOFS"
*
*------------------------------------------------------------------------------
*
*** Contents of Outer Field Cage structureure TOFS:  Copper/Kapton/Nomex/Adhesive
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!//   TGeoVolume *TOKA = gGeoManager->MakeTube("TOKA",GetMed("TPCE_MYLAR"),tokaIR,tokaOR,TPCG_dzYF2);
!//   TOKA->SetTitle("KAPTON layer");
!//   TOKA->SetLineColor(kGreen);
!//   //  TOKA->SetVisibility(1);
!//   TOFC->AddNode(TOKA,1,gGeoIdentity);
Block TOKA  is  MYLAR layer
      material  mylar     " should be close to kapton "
      Attribute TOKA      seen=1  colo=kGreen
      SHAPE     TUBE      rmin=tokaIR  rmax=tokaOR dz=TPCG_dzYF2
endBlock        "end TOKA"
*
*------------------------------------------------------------------------------
*
!//   //  TGeoVolume *TONX = gGeoManager->MakeTube("TONX",GetMed("TPCE_NOMEX"),tonxIR,tonxOR,TPCG_dzYF3);
!//   TGeoVolume *TONX = gGeoManager->MakePcon("TONX",GetMed("TPCE_NOMEX"),0,360,6);
!//   pcon = (TGeoPcon *)  TONX->GetShape();
!//   pcon->DefineSection(0,-TPCG_dzYF2 , tonxOR-0.2,tonxOR);
!//   pcon->DefineSection(1,-TPCG_dzYF3, tonxOR-0.2,tonxOR);
!//   pcon->DefineSection(2,-TPCG_dzYF3, tonxIR,tonxOR);
!//   pcon->DefineSection(3, TPCG_dzYF3, tonxIR,tonxOR);
!//   pcon->DefineSection(4, TPCG_dzYF3, tonxOR-0.2,tonxOR);
!//   pcon->DefineSection(5, TPCG_dzYF2 , tonxOR-0.2,tonxOR);
!//   TONX->SetTitle("Nomex support");
!//   TONX->SetLineColor(kBlack);
!//   //  TONX->SetVisibility(1);
!//   TOFC->AddNode(TONX,1,gGeoIdentity);
Block TONX  is  Nomex support
      material  Nomex
      Attribute TONX      seen=1  colo=kRed
      SHAPE     PCON      Phi1=0  Dphi=360  Nz=6,
                          zi ={-TPCG_dzYF2, -TPCG_dzYF3,-TPCG_dzYF3,TPCG_dzYF3,TPCG_dzYF3,TPCG_dzYF2},
                          Rmn={tonxOR-0.2 ,tonxOR-0.2  ,tonxIR     ,tonxIR    ,tonxOR-0.2,tonxOR-0.2},
                          Rmx={tonxOR     ,tonxOR      ,tonxOR     ,tonxOR    ,tonxOR    ,tonxOR    }
endBlock        "end TONX"
*
*------------------------------------------------------------------------------
*
!//   TGeoVolume *TOAD = gGeoManager->MakeTube("TOAD",GetMed("TPCE_MYLAR"),toadIR,toadOR,TPCG_dzYF2);
!//   TOAD->SetTitle("Adhesive layer");
!//   TOAD->SetLineColor(kViolet);
!//   //  TOAD->SetVisibility(1);
!//   TOFC->AddNode(TOAD,1,gGeoIdentity);
Block TOAD  is  Adhesive layer
      material  mylar     " should be close ? "
      Attribute TOAD      seen=1  colo=kLightBlue
      SHAPE     TUBE      rmin=toadIR rmax=toadOR dz=TPCG_dzYF2
endBlock        "end TOAD"

*
*------------------------------------------------------------------------------
*
Block TOIG  is Insulating Gas (Nitrogen)
!//   TGeoVolume *TOIG = gGeoManager->MakeTube("TOIG",GetMed("TPCE_NITROGEN_GAS"),toigIR,toigOR,TPCG_dzYF2);
!//   TOIG->SetTitle("Insulating Gas");
!//   TOIG->SetLineColor(kBlack);
!//  TOIG->SetVisibility(1);
!//   TOFC->AddNode(TOIG,1,gGeoIdentity);
      material  NITROGEN_GAS
      Attribute TOAD      seen=1  colo=kLightBlue
      SHAPE     TUBE      rmin=toigIR rmax=toigOR dz=TPCG_dzYF2
endBlock        "end TOIG"
*
*------------------------------------------------------------------------------
*

!//   // Gas Containment Vessel (Al) + HA
!//
!//   TGeoVolume *TOHA = gGeoManager->MakeTube("TOHA",GetMed("TPCE_AL_HONEYCOMB"),tohaIR,tohaOR,TPCG_dzYF3);
!//   TOHA->SetTitle("Honeycomb/Adhesive mixture");
!//   TOHA->SetLineColor(kViolet);
!//   //  TOHA->SetVisibility(1);
!//   TOFC->AddNode(TOHA,1,gGeoIdentity);
Block TOHA  Gas Containment Vessel (Al) + HA
      material  AL_HONEYCOMB
      Attribute TOHA      seen=1  colo=kViolet
      SHAPE     TUBE      rmin=tohaIR rmax=tohaOR dz=TPCG_dzYF3
endBlock        "end TOHA"
*
*------------------------------------------------------------------------------
*
!//   //________________________________________________________________________________
!//   // TpcSectorWhole is Sector as Whole
!//   //________________________________________________________________________________
!//   TGeoVolume *TpcSectorWhole = gGeoManager->MakePcon("TpcSectorWhole",GetMed("TPCE_STANDARD"),-15,30,8);
!//   pcon = (TGeoPcon *) TpcSectorWhole ->GetShape();
!//   pcon->DefineSection(0,  TPCG_MembTHK/2, tifcOR                  ,tofcIR       ); // membrane
!//   pcon->DefineSection(1,         zWheel1, tifcOR                  ,tofcIR       ); // gas volume + sector
!//   pcon->DefineSection(2,         zWheel1, tifcOR                  ,TPCG_WheelOR1); // wheel begins
!//   pcon->DefineSection(3,        zTOFCend, tifcOR                  ,TPCG_WheelOR1);
!//   pcon->DefineSection(4,        zTOFCend, tifcOR                  ,TPCG_WheelOR ); // wheel with TOFC
!//   pcon->DefineSection(5,zTIFCFlangeBegin, tifcOR                  ,TPCG_WheelOR ); // wheel with TIFC flange
!//   pcon->DefineSection(6,zTIFCFlangeBegin, TPCG_tifcRF+TPCG_tifcDRT,TPCG_WheelOR );
!//   pcon->DefineSection(7,  TPCG_LengthT/2, TPCG_tifcRF+TPCG_tifcDRT,TPCG_WheelOR );
!//   TpcSectorWhole->SetTitle("Tpc Sector as Whole");
!//   //  TpcSectorWhole->SetVisibility(1);
!//   TpcSectorWhole->SetLineColor(kBlack);

*
*------------------------------------------------------------------------------
*
Block TSWH  TpcSectorWhole is Sector as Whole
      medium  STANDARD
      Attribute TSWH      seen=1  colo=kViolet
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=8,
      zi ={TPCG_MembTHK/2,zWheel1                 ,zWheel1                 ,zTOFCend,
           zTOFCend      ,zTIFCFlangeBegin        ,zTIFCFlangeBegin        ,TPCG_DzEnvelop},
      Rmn={tifcOR        ,tifcOR                  ,tifcOR                  ,tifcOR,
           tifcOR        ,TPCG_tifcRF+TPCG_tifcDRT,TPCG_tifcRF+TPCG_tifcDRT,TPCG_tifcRF+TPCG_tifcDRT},
      Rmx={tofcIR        ,tofcIR                  ,TPCG_WheelOR1           ,TPCG_WheelOR1,
           TPCG_WheelOR  ,TPCG_WheelOR            ,TPCG_WheelOR            ,TPCG_WheelOR}

      Create and Position TSAW Konly='Many'

endBlock        "end TSWH"

*
*------------------------------------------------------------------------------
*
Block TSAW  TpcSectorAndWheel
      medium  STANDARD
      Attribute TSAW      seen=1  colo=kYellow
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=2,
      zi = {204,TPCG_DzEnvelop-TPCG_MembTHK/2}, Rmn={48,48}, Rmx={208,208}
!//   zi = {204,274}, Rmn={48,48}, Rmx={208,208}




!// Drawing 24A3685B height =  27.373
        dX2 = 1.470;
        dX  = 0;
!//write(*,*) '************** USE TECW sec=1 ************';
        USE TECW sec=1;
        RIBI =  {    dX                 ,dX2                            , 0.,0.         !// r for upper edge
                  4.261                 ,0.375                          , 0.,0.
                 10.561                 ,0.375                          , 0.,0.
                 16.861                 ,0.375                          , 0.,0.
                 23.161                 ,0.375                          , 0.,0.
                 25.950                 ,TECW_Height/INCH-25.950        , 0.,0.
                 4.261-1.070/2          ,1.070                          ,-5.645,0.750   !// r for center
                 4.261-1.070/2          ,1.070                          , 5.645,0.750   !// r for center
                 10.561 -1.070/2        ,1.070                          ,-3.895,0.750   !// r for center
                 10.561 -1.070/2        ,1.070                          , 3.895,0.750   !// r for center
                 1.050+2.515+21.915     ,2*(25.950-(1.050+2.515+21.915)), 0.,2       }; !// r for center

 !// Drawing 24A3925G
        RIBO = { 0.280,                 1.5480 - 0.280,         0, 0            !// r for upper edge
                 4.261,                 0.375,                  0, 0
                 7.410,                 0.375,                  0, 0
                10.560,                 0.375,                  0, 0
                13.710,                 0.375,                  0, 0
                16.859,                 0.375,                  0, 0
                20.009,                 0.375,                  0, 0
                23.158,                 0.375,                  0, 0
                26.309,                 27.595+0.28-26.309,     0, 0
                ( 7.410+ 4.261+0.375)/2,( 7.410- 4.261-0.375), 17.250/2, 1.250  !// r for center
                ( 7.410+ 4.261+0.375)/2,( 7.410- 4.261-0.375),-17.250/2, 1.250
                (13.710+10.560+0.375)/2,(13.710-10.560-0.375),  0, 0.375
                (16.859+13.710+0.375)/2,(16.859-13.710-0.375),  0, 0.375
                26.309-1.575/2         , 1.575               ,  0, (26.309-6.565-18.960)*2};

  call vzero (yhI, 160);
  i = 0; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.180*(-2.5 + j);}
  i = 1; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.764*(-2.0 + j);}
  i = 2; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.553*(-2.0 + j);}
  i = 3; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) = 13.368*(-0.5 + j);}
  i = 4; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.131*(-2.0 + j);}
  i = 5;
  yhI(9*i+0) = - 3.892 - 3.896/2;  yhI(9*i+3) = - yhI(9*i+0);
  yhI(9*i+1) =         - 3.896/2;  yhI(9*i+2) = - yhI(9*i+1);
  i = 6;
  yhI(9*i+0) = - 3.611 - 3.614/2;  yhI(9*i+3) = - yhI(9*i+0);
  yhI(9*i+1) =         - 3.614/2;  yhI(9*i+2) = - yhI(9*i+1);
  i = 7;
  yhI(9*i+0) = - 3.281 - 3.429/2;  yhI(9*i+3) = - yhI(9*i+0);
  yhI(9*i+1) =         - 3.429/2;  yhI(9*i+2) = - yhI(9*i+1);
  i = 8; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.150*(-1.5 + j);}
  i = 9; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  4.152*(-1.0 + j);}
  i =10; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.730*(-1.0 + j);}
  i =11; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.308*(-1.0 + j);}
  i =12; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  3.075*(-1.0 + j);}
  i =13; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  4.927*(-0.5 + j);}
  i =14; do j = 0,noHolesPerRowI(i)-1 {yhI(9*i+j) =  4.083*(-0.5 + j);}

yhOF = {-15.025, -11.606, -8.177, -4.220,  0,  4.220,  8.177,  11.606, 15.025,
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
        -9.495,   -6.330, -3.165,              3.165,  6.330,   9.495,  0,  0, 0};

  do inOut = 0,1
  {
!//    tpcPadPlane[inOut] = new TGeoVolumeAssembly("TpcPadPlane");
!//    tpcPadPlane[inOut]->SetTitle(Form("Tpc%sPadPlane",InnerOuter[inOut]));

!//write(*,*) '************ use TECW sec=(inOut+1)=',(inOut+1),' ********';
    use TECW sec=(inOut+1)
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
    alpha = 180./Pi*ATan(dYdX);
    r1 = y1/dYdX;
    r2 = y2/dYdX;
    dRSectorShift = TECW_rMin - r1;

!//             Holes ignore radii

    if (inOut == 0) {
      xHoles(0) = r2 - 1.988*INCH;
      do i = 1,noHolesRows(inOut)-1 { xHoles(i) = xHoles(i-1) - 1.575*INCH;}
    } else {
      do i = 0,noHolesRows(inOut)-1 { xHoles(i) = r2 - (1.988 + 1.575*i)*INCH;}
    }

    Create And Position TSAS kOnly='MANY'

!//    tpcWheel[inOut] = new TGeoVolumeAssembly(Form("TpcWheel%sAssembly",InnerOuter[inOut]));
!//    tpcWheel[inOut]->SetTitle(Form("the %s wheel assembly",InnerOuter[inOut]));
    Create And Position TWAS kOnly='MANY'

!//  TGeoVolume *WheelRibBox = gGeoManager->MakeBox("WheelRibBox",GetMed("TPCE_ALUMINIUM"), TPCG_WheelBoxDy/2, TPCG_WheelBoxDx/2, TPCG_WheelTHK/2);
!//  TpcSectorAndWheel->AddNodeOverlap(WheelRibBox, 1, new TGeoTranslation(TPCG_WheelR1, 0., zWheel1+TPCG_WheelTHK/2));

        Create And Position TBRW          X=TPCG_WheelR1, Z=zWheel1+TPCG_WheelTHK/2 "//TpcWheelRibBox"
    x1 = TPCG_WheelR0*cos15;
    x2 = TPCG_WheelR2;
    dz = TPCG_WheelRibHeight/2;
    z  = zWheel1 + dz;
    dx = (x2 - x1)/2;
    dy = TPCG_WheelTotalRibWidth/4;
    x  = (x1 + x2)/2;
    y  = (x *tan15 - dy)*(2*inOut-1);
!//    wheelRib = gGeoManager->MakePara("TpcWheelRib", GetMed("TPCE_ALUMINIUM"), dy, dx, dz,-(1-2*inOut)*15, 0, 0);
!//    TpcSectorAndWheel->AddNodeOverlap(wheelRib,3+inOut, new TGeoCombiTrans(x, y, z,GetRot("YZXZ")));
    myDx=dy; myDy=dx; myDz=dz; myAlph=-(1-2*inOut)*15;
    Create  And Position TWRB x=x, y=y, z=z ORT=YX-Z

    dz = TPCG_WheelTHK/2;
    z  = zWheel1 + dz;
    dy = TPCG_WheelRibWidth/4;
    x  = (x1 + x2)/2;
    y  = (x *tan15 - dy)*(2*inOut-1);
!//    wheelRib = gGeoManager->MakePara("TpcWheelRib", GetMed("TPCE_ALUMINIUM"), dy, dx, dz,-(1-2*inOut)*15, 0, 0);
!//    TpcSectorAndWheel->AddNodeOverlap(wheelRib,5+inOut, new TGeoCombiTrans(x, y, z,GetRot("YZXZ")));
    myDx=dy; myDy=dx; myDz=dz; myAlph=-(1-2*inOut)*15;
    Create  And Position TWRB x=x, y=y, z=z ORT=YX-Z


  }
!//    TString rotm("R345");
!//    if (inOut == 1) rotm = "R015";
  do alpha = -15,15,30
  {
    do j = 0,3
    {
      if (j == 0) r = TPCG_WheelR2 - 1.5/8.5*(TPCG_WheelR2 - TPCG_WheelR1);
      if (j == 1) r = TPCG_WheelR2 - 6.5/8.5*(TPCG_WheelR2 - TPCG_WheelR1);
      if (j == 2) r = TPCG_WheelR1 - 2.0/8.5*(TPCG_WheelR1 - TPCG_WheelR0);
      if (j == 3) r = TPCG_WheelR1 - 7.0/8.5*(TPCG_WheelR1 - TPCG_WheelR0);
      x = r ;
      y = r *tand(alpha);
!//      TpcSectorAndWheel->AddNodeOverlap(WheelRibBox, 3+4*inOut+j, new TGeoCombiTrans(x, y, zWheel1+TPCG_WheelTHK/2, GetRot(rotm)));
     Position TBRW x=x y=y z=zWheel1+TPCG_WheelTHK/2 alphaz=alpha 
    }
    qwe(1) = -qwe(1);
    qwe(2) = -qwe(2);
  }

  Create And Position TWBT                      "//WheelBottom"
  Create And Position TWRI      kOnly='MANY'    "//WheelOuterRing"
  Create And Position TWTR                      "//pcWheelTopRib"
  Create And Position TWMR                      "//pcWheelMidleRib"
  Create And Position TRDO      kOnly='MANY'    "//RDOAssembly"



  RCoolingTube = TPCG_WheelR2 - TPCG_heigTube - 2*TPCG_dxRDO;
  do iCoo = 0,7
  {
    dz = TPCG_heigTube*cm/2;
    dy = TPCG_widTube *cm/2;
    z  = RCoolingTube;
    dx1 = (z-dz)*tan15 - 2*TPCG_RDOCoolingdY;
    dx2 = (z+dz)*tan15 - 2*TPCG_RDOCoolingdY;
!//    TGeoVolume *coolingTube = gGeoManager->MakeTrd1("CoolingTube", GetMed("TPCE_Water_Pipe"), dx1, dx2, dy, dz);
!//    TpcSectorAndWheel->AddNode(coolingTube,iCoo+3, new TGeoCombiTrans(z, 0, TPCG_zRDO - 2*dy, GetRot("90XD")));
    mySha='TRD1';
    myPar = {dx1,dx2,dy,dz};
    Create and Position TCOO x=z z=(TPCG_zRDO-2*dy) ORT=YZX
    RCoolingTube -= dRDOCooling(iCoo);
  }

!//             Cables
  dz = 4.5;
  dy = 4.5/2;
  z  = TPCG_zRDO + TPCG_dzRDO + dz;
  x1 = TPCG_WheelR0;
  x2 = TPCG_WheelR2;
  dx = (x2 - x1)/2;
  x  = (x1 + x2)/2;
  do iCab = 0,1
  {
    y  = - (x *tan15 - dy) * (1 - 2*iCab);
!//    TGeoVolume *cables = gGeoManager->MakePara("Cables", GetMed("Cables"), dy, dx, dz,-15*(1 - 2*iCab), 0, 0);
!//    TpcSectorAndWheel->AddNode(cables,iCab+1, new TGeoCombiTrans( x, y, z,GetRot("YZXZ")));
    myPar={dy, dx, dz,-15*(1 - 2*iCab), 0, 0};
    Create and Position TCAB x=x y=y z=z ORT=YX-Z
  }
endBlock        "end TSAW"
*
*------------------------------------------------------------------------------
*
Block TSAS  TpcInnerSectorAssembly &  TpcOuterSectorAssembly TSAS and TSA1
       SHAPE     PGON    Phi1=-15  Dphi=30  Nz=2, nPDV=1,
       zi = {zzzSA(1),zzzSA(2)}, Rmn={rmnSA(inOut),rmnSA(inOut)},
                                 Rmx={rmxSA(inOut),rmxSA(inOut)}
       Attribute TSAS  seen=0 
       Create and Position TALS         "!//TpcSectorAlSupport"
       Create and Position TSGT         "!//TpcSectorG10 (GTen)"
!// Ribs
    dy = (r2 - r1)/2;
    y  = (r2 + r1)/2;
    dx = TECW_widthRib/2;
    dz = (TECW_Thick - TPCG_PadPlaneThickness - TECW_ThickAl)/2;
    z  = z2 - dz;
    dxW = TPCG_WireMountWidth/cos15/2;
    dzW = TPCG_WireMountHeight/2;
    xW = 0;
    do iRib = 0,1
    {
!//      TGeoVolume *trib = gGeoManager->MakePara("TRIB", GetMed("TPCE_ALUMINIUM"), dx, dy, dz,-(1-2*iRib)*alpha, 0, 0);
      mySha = 'PARA';
      myPar = {dx, dy, dz, -(1-2*iRib)*alpha};
      Create TRIB;
!//      tpcSector[inOut]->AddNode(trib,iRib+1, new TGeoCombiTrans(y,-x, z,GetRot("YZXZ")));
      x  = ((y1 + y2)/2 - dx)*(1-2*iRib);
      Position TRIB x=y y=-x z=z ORT=YX-Z

!//VP      xW = ((y1 + y2)/2 +  dxW)*(1-2*iRib);
      xW = ((y1 + y2)/2 -  dxW)*(1-2*iRib);
!//      TGeoVolume *wireMount = gGeoManager->MakePara("WireMount", GetMed("TPCE_G10"), dxW, dy, dzW,-(1-2*iRib)*alpha, 0, 0);
      myPar = {dxW, dy, dzW, -(1-2*iRib)*alpha, 0, 0};
      Create TWIR;
      zW = z1 - dzW/2;
!//      TGeoCombiTrans *rt = new TGeoCombiTrans(y,-xW, zW,GetRot("YZXZ"));
!//      tpcSector[inOut]->AddNode(wireMount,iRib+1, rt);
      Position TWIR x=y y=-xW z=zW ORT=YX-Z

    }
!//    Char_t *io = InnerOuter[inOut];
!//    new TGeoCompositeShape(Form("%sSector",io),
!//                        Form("%sSectorItSelf + WireMount%sLeft:WireMountTR%sLeft + WireMount%sRight:WireMountTR%sRight",
!//                             io,io,io,io,io));
!//
    do iRib = 0, TECW_noribs-1
    {
!//      r  = r2 - RIB[inOut][iRib].x*INCH;
!//      dr = RIB[inOut][iRib].dx*INCH;
!//      y  = RIB[inOut][iRib].y *INCH;
!//      dy = RIB[inOut][iRib].dy*INCH;
      r  = r2 - RIB(kXXrib,iRib,inOut)*INCH;
      dr = RIB(kDXrib,iRib,inOut)*INCH;
      y  = RIB(kYYrib,iRib,inOut)*INCH;
      dy = RIB(kDYrib,iRib,inOut)*INCH;

!//  write(*,*)  '##RIBS## rib iRib = ' ,iRib , '  r = ',r,'  dr = ',dr, '  y = ',y,' dy = ',dy;
      if (dy < 1.e-7) {
        dx2 =  r      *dYdX - TECW_widthRib - 2*dxW; !//TECW_widthLip;
        dx1 = (r - dr)*dYdX - TECW_widthRib - 2*dxW; !//TECW_widthLip;
        xc = r - TECW_zStepRib/2;
!//TGeoVolume *trib = gGeoManager->MakeTrd1("TRIB", GetMed("TPCE_ALUMINIUM"), dx1, dx2, dz, dr/2);
!//tpcSector[inOut]->AddNode(trib,iRib+3, new TGeoCombiTrans(r-dr/2, 0, z2-dz, GetRot("90XD")));
        mySha='TRD1';
        myPar = {dx1, dx2, dz, dr/2};
        Create And Position TRIB x=r-dr/2 y=0 z=z2-dz ORT=YZX
      } else {
!//TGeoVolume *trib = gGeoManager->MakeBox("TRIB", GetMed("TPCE_ALUMINIUM"), dr/2, dy/2, dz);
!//tpcSector[inOut]->AddNode(trib,iRib+3,new TGeoTranslation(r,y, z2-dz));
        mySha='BOX ';
        myPar = {dr/2, dy/2, dz};
        Create And Position TRIB x=r y=y z=z2-dz
      }
    }

endBlock        "end TSAS"

*
*------------------------------------------------------------------------------
*
Block TWAS  TpcWheelInnerAssembly & TpcWheelOuterAssembly     TWAS and TWA1
       SHAPE     PGON    Phi1=-15  Dphi=30  Nz=2, nPDV=1,
       zi = {zzzWA(1),zzzWA(2)}, Rmn={rmnWA(inOut),rmnWA(inOut)},
                                 Rmx={rmxWA(inOut),rmxWA(inOut)}
       Attribute TWAS  seen=0 

!// put holes, cooling tube and FEE
!//    cout << "put holes in " << noHolesRows(inOut) << " rows for sector\t" << inOut << endl;
    dx =   TPCG_widTube*cm/2;
    dz =   depthIO(inOut)/2;
    dy =   heightIO(inOut)/2;
    y1 = - dy;
    y2 =   dy;
    x1 =  width1IO(inOut)/2 - dx;
    x2 =  width2IO(inOut)/2 - dx;
    x0 =  (y1*x2-y2*x1)/(y2-y1);
    dYdX  =  (x2 - x1)/(y2 - y1);
    beta = 180./Pi*ATan(dYdX);
!//   write(*,*) ' x0=' ,x0 , ' dYdX= ' , dYdX , ' beta= ' , beta;
    if (inOut == 1) dy -= 0.7;
    y  = xHoles(0) - dy;
    x = (x1 + x2)/2;
    do iCoo = 0,1
    {
!//      coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), dx, dy, dz,-(1-2*iCoo)*beta, 0, 0);
!//      tpcWheel(inOut)->AddNode(coolingTube, 2*inOut+iCoo+1, new TGeoCombiTrans(Y,x*(2*iCoo-1),zWheel1+ dz,GetRot("YZXZ")));
      myPar={dx, dy, dz,-(1-2*iCoo)*beta, 0, 0};
      mySha = 'PARA';
      Create  and Position TCOO  x=y, y=x*(2*iCoo-1), z=zWheel1+dz ORT=YX-Z;
    }

    do iCoo = 0,noHolesRows(inOut)-1
    {
      x = xHoles(iCoo);         !// cm
!//  Write(*,*) 'inOut = ' , inOut , 'hole ' , iCoo , 'X = ' , x ;
      dx = TPCG_heigTube*cm/2;
      dz = TPCG_widTube*cm/2;
      dy = CoolingTubeLength(iCoo,inOut)*INCH/2;
      if (inOut == 0)   {dy2 = dy + 2*dYdX*dx;dy1 = dy;            }
      else              {dy2 = dy            ;dy1 = dy - 2*dYdX*dx;}

!//     coolingTube = gGeoManager->MakeTrd1("CoolingTube", GetMed("TPCE_Water_Pipe"), dy1, dy2, dz, dx);
!//     tpcWheel(inOut)->AddNode(coolingTube, 2*iCoo+inOut+5, // "90XD" : (x,y,z) => ( y, z, x)
!//     new TGeoCombiTrans(x-dx-TFEE_AssemblyThickness/2+dRSectorShift,0, zWheel1+ dz,GetRot("90XD")));
      mySha='TRD1';
      myPar= {dy1, dy2, dz, dx};
      myPar(11) = x-dx-TFEE_AssemblyThickness/2+dRSectorShift;
      myPar(13) = zWheel1+dz;
      Create And Position TCOO x=myPar(11) z=myPar(13) ORT=YZX


      do jCoo = 0,noHolesPerRow(iCoo,inOut)-1
      {
!//     tpcWheel[sec]->AddNode(FEE,2*(9*i+j)+sec+1,
!//                            new TGeoTranslation(X+FEEAssemblyThickness/2+dRSectorShift,Y,zWheel1+ 2*dz+TFEERibDZ));
       y = yHoles(9*iCoo+jCoo,InOut)*INCH;
       Create And Position FEEA X=x+TFEE_AssemblyThickness/2+dRSectorShift,
                                Y=y,
                                Z=zWheel1+2*dz+TFEE_RibDZ Konly='Many'
      }
    }

endBlock        "end TWAS"
*
*------------------------------------------------------------------------------
*
Block FEEA  TGeoVolumeAssembly(FEE)
!//TGeoVolumeAssembly *FEE = new TGeoVolumeAssembly("FEE"); // Weight = 181*FEE = 26 kG (A.Lebedev)
      Attribute FEEA      seen=1  colo=kGreen
      SHAPE     BOX    dX=TFEE_Ass(1), dY=TFEE_Ass(2), dZ=TFEE_Ass(3)
      Attribute FEEA  seen=0 



!//FEE->AddNode(FEEplate,  1, gGeoIdentity);                // Cables = 9 lb ( - " -)
!//FEE->AddNode(FEERib,    1, new TGeoTranslation(-FEERibDX-FeeCardDX,0,0));
!//FEE->AddNode(FEEitself, 1, new TGeoTranslation(2*FeeCardDX,0,0.5));

      Create And Position FEEP
      Create And Position FEER x=-TFEE_RibDX-TFEE_CardDX
      Create And Position FEEI x=2*TFEE_CardDX z=0.5



endBlock        "end FEEA"
*
*------------------------------------------------------------------------------
*
Block FEEP  FEEplate
!//TGeoVolume *FEEplate = gGeoManager->MakeBox("FEEPlate",  GetMed("TPCE_ALUMINIUM"), FeePlateDX, FeePlateDY, FeePlateDZ);
      Attribute FEEP      seen=1  colo=kRed
      Material ALUMINIUM
      SHAPE     BOX    dX=TFEE_PlateDX dY=TFEE_PlateDY dZ=TFEE_PlateDZ
endBlock        "end FEEP"
*
*------------------------------------------------------------------------------
*
Block FEER  FEERib
!//TGeoVolume *FEERib   = gGeoManager->MakeBox("FEERib",    GetMed("TPCE_ALUMINIUM"), FEERibDX,  FEERibDY,  FEERibDZ);
      Attribute FEER      seen=1  colo=1
      Material ALUMINIUM
      SHAPE     BOX    dX=TFEE_RibDX  dY=TFEE_RibDY  dZ=TFEE_RibDZ
endBlock        "end FEER"
*
*------------------------------------------------------------------------------
*


Block FEEI  FEEitself
!// TGeoVolume *FEEitself= gGeoManager->MakeBox("FEEitself", GetMed("TPCE_G10"),       FeeCardDX, FeeCardDY, FeeCardDZ);
      Attribute FEEI      seen=1  colo=kViolet
      Material G10
      SHAPE     BOX  dX=TFEE_CardDX dY=TFEE_CardDY dZ=TFEE_CardDZ
endBlock        "end FEEI"
*
*------------------------------------------------------------------------------
*
block TALS TpcSectorAlSupport  the Sector G10 alliminium support
*!//     TGeoVolume *tpcSectorAlSupport = gGeoManager->MakePgon("TpcSectorAlSupport",GetMed("TPCE_ALUMINIUM"),-alpha,2*alpha,1,2);
*!//     pgon->DefineSection(0, z1+TPCG_PadPlaneThickness             , r1, r2);
*!//     pgon->DefineSection(1, z1+TPCG_PadPlaneThickness+TECW_ThickAl, r1, r2);
      Attribute TALS seen=1  colo=kYellow;
      Material ALUMINIUM;
      SHAPE PGON    Phi1=-alpha  Dphi=2*alpha  Nz=2, nPDV=1,
      zi = {z1+TPCG_PadPlaneThickness,z1+TPCG_PadPlaneThickness+TECW_ThickAl},
      Rmn={r1,r1}, Rmx={r2,r2};

!//    dz = TECW[sec].ThickAl/2;
!//    TGeoVolume *thole = gGeoManager->MakeBox("THOLE", GetMed("TPCE_STANDARD"), dx, dy, dz);
      myPar(1) = TECW_holeDx;
      myPar(2) = TECW_holeDy;
      myPar(3) = TECW_ThickAl/2;
 !//    z1 = zWheel1 - TECW[inOut].Thick;
    myPar(13) = zWheel1 - TECW_Thick + TPCG_PadPlaneThickness + TECW_ThickAl/2;
    do iTALS = 0, noHolesRows(inOut)-1
    {
      myPar(11) = xHoles(iTALS); !// cm
      do jTALS = 0,noHolesPerRow(iTALS,inOut)-1
      {
        myPar(12) = yHoles(9*iTALS+jTALS,inOut)*INCH;
!//     tpcSectorAlSupport->AddNode(thole,9*iTALS+jTALS+1, new TGeoTranslation(X,Y,z1+PadPlaneThickness+TECW[inOut].ThickAl/2));
        Create And Position THOL x=myPar(11) y=myPar(12) z=myPar(13)
      }
    }



endBlock        "end TALS"
*
*------------------------------------------------------------------------------
*
block TSGT TpcSectorG10    the Sector G10
!//     TGeoVolume *tpcSectorG10 = gGeoManager->MakePgon("TpcSectorG10",GetMed("TPCE_G10"),-alpha,2*alpha,1,2);
!//     pgon->DefineSection(0, z1                  , r1, r2);
!//     pgon->DefineSection(1, z1+TPCG_PadPlaneThickness, r1, r2);
!//     TpcSectorG10->AddVolume(tpcSectorG10);
!//     tpcSector[inOut]->AddNode(tpcSectorG10,1,gGeoIdentity);
      Attribute TSGT      seen=1  colo=kBlue
      Material G10
      SHAPE     PGON    Phi1=-alpha  Dphi=2*alpha  Nz=2, nPDV=1,
      zi = {z1,z1+TPCG_PadPlaneThickness},
      Rmn={r1,r1}, Rmx={r2,r2}
endBlock        "end TSGT"
*
*------------------------------------------------------------------------------
*
block TWBT //WheelBottom
!!!!yf  TGeoVolume *WheelBottom = gGeoManager->MakePcon("WheelBottom",GetMed("TPCE_ALUMINIUM"),-15,30,4);
!!!!yf  pcon = (TGeoPcon *) WheelBottom ->GetShape();
!!!!yf  pcon->DefineSection(0,           zWheel1,     TPCG_WheelIR, TPCG_WheelR0); // wheel begins
!!!!yf  pcon->DefineSection(1,zWheel1+3.500*INCH,     TPCG_WheelIR, TPCG_WheelR0); // wheel begins
!!!!yf  pcon->DefineSection(2,zWheel1+3.500*INCH, TPCG_WheelIR+3.6, TPCG_WheelR0); // wheel begins
!!!!yf  pcon->DefineSection(3,    TPCG_LengthW/2, TPCG_WheelIR+3.6, TPCG_WheelR0); // wheel begins
!!!!yf  TpcSectorAndWheel->AddNode(WheelBottom,1,gGeoIdentity);
      Attribute TWBT      seen=1  colo=kGreen
      Material  ALUMINIUM
      SHAPE PCON    Phi1=-15  Dphi=30  Nz=4,
      zi ={zWheel1      ,zWheel1+3.500*INCH,zWheel1+3.500*INCH,TPCG_LengthW/2  },
      Rmn={TPCG_WheelIR ,TPCG_WheelIR      ,TPCG_WheelIR+3.6  ,TPCG_WheelIR+3.6},
          Rmx={TPCG_WheelR0 ,TPCG_WheelR0      ,TPCG_WheelR0      ,TPCG_WheelR0    }

endBlock        "end TWBT"
*
*------------------------------------------------------------------------------
*
block TWRI      WheelOuterRing
Attribute TWRI seen=0  colo=kGreen
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=4,
                zi ={zWheel1      ,zTOFCend     ,zTOFCend    ,TPCG_LengthW/2},
                Rmn={TPCG_WheelR2*cos15 ,TPCG_WheelR2*cos15 ,TPCG_WheelR2*cos15,TPCG_WheelR2*cos15  },
                Rmx={TPCG_WheelOR1      ,TPCG_WheelOR1      ,TPCG_WheelOR      ,TPCG_WheelOR        }

           Create And Position TWRC

           myCos = cos15; myRmin=TPCG_WheelR2; myRmax = TPCG_WheelOR1;
           myG1 = myRmin*cos15;

       nCount = 0;
           loop {
         nCount+=1;
             myG2 = myRmin;
             if (myG2 .gt.myRMax*myCos ) myG2 = myRMax*myCos ;
         alpha = acos(myCos)/Pi*180
                 Create and Position TWRG
         myCos = myG2/myRmin; myG1=myG2;
         if (nCount .gt.10) break;
                 } until ( myG2+1e-4 .ge. myRmin)

endBlock        "end TWRI"
*
*------------------------------------------------------------------------------
*
block TWRG WheelOuterRing PGON part
Attribute TWRG seen=1  colo=kGreen
Material ALUMINIUM
      SHAPE     PGON    Phi1=-alpha  Dphi=alpha*2  Nz=2 Npdv=1,
       zi ={zWheel1,TPCG_LengthW/2},
       Rmn={myG1   ,myG1          },
       Rmx={myG2   ,myG2          }
endBlock        "end TWRG"
*
*------------------------------------------------------------------------------
*
block TWRC WheelOuterRing PCON part
Attribute TWRC seen=1  colo=kGreen
Material ALUMINIUM
      SHAPE     PCON    Phi1=-15  Dphi=30  Nz=4,
       zi ={zWheel1           ,zTOFCend          ,zTOFCend          ,TPCG_LengthW/2    },
           Rmn={TPCG_WheelR2/cos15,TPCG_WheelR2/cos15,TPCG_WheelR2/cos15,TPCG_WheelR2/cos15},
       Rmx={TPCG_WheelOR1     ,TPCG_WheelOR1     ,TPCG_WheelOR      ,TPCG_WheelOR      }
endBlock        "end TWRC"
*
*------------------------------------------------------------------------------
*
block TWTR      TpcWheelTopRib
!!!!yf  wheelRib = gGeoManager->MakePgon("TpcWheelTopRib",GetMed("TPCE_ALUMINIUM"), -15, 30,1,2);
!!!!yf  pgon = (TGeoPgon *) wheelRib->GetShape();
!!!!yf  pgon->DefineSection(0,zWheel1                    , TPCG_WheelR2 - (TPCG_WheelTotalRibWidth-TPCG_WheelRibWidth)/2, TPCG_WheelR2);
!!!!yf  pgon->DefineSection(1,zWheel1+TPCG_WheelRibHeight, TPCG_WheelR2 - (TPCG_WheelTotalRibWidth-TPCG_WheelRibWidth)/2, TPCG_WheelR2);

Attribute TWTR seen=1  colo=kBlue
Material ALUMINIUM
      myG1=TPCG_WheelR2 - (TPCG_WheelTotalRibWidth-TPCG_WheelRibWidth)/2;
      SHAPE  PGON    Phi1=-15  Dphi=30  Nz=2 Npdv=1,
       zi ={zWheel1     ,zWheel1+TPCG_WheelRibHeight},
           Rmn={myG1        ,myG1},
       Rmx={TPCG_WheelR2,TPCG_WheelR2}
endBlock        "end TWTR"
*
*------------------------------------------------------------------------------
*
block TWMR TpcWheelMiddleRib
!!!!yf  wheelRib = gGeoManager->MakePgon("TpcWheelMiddleRib",GetMed("TPCE_ALUMINIUM"), -15, 30,1,4);
!!!!yf  pgon->DefineSection(0,zWheel1                    , TPCG_WheelR1 - TPCG_WheelTotalRibWidth/2, TPCG_WheelR1 + TPCG_WheelTotalRibWidth/2);
!!!!yf  pgon->DefineSection(1,zWheel1+TPCG_WheelRibHeight, TPCG_WheelR1 - TPCG_WheelTotalRibWidth/2, TPCG_WheelR1 + TPCG_WheelTotalRibWidth/2);
!!!!yf  pgon->DefineSection(2,zWheel1+TPCG_WheelRibHeight, TPCG_WheelR1 - TPCG_WheelRibWidth/2     , TPCG_WheelR1 + TPCG_WheelRibWidth/2);
!!!!yf  pgon->DefineSection(3,TPCG_LengthW/2             , TPCG_WheelR1 - TPCG_WheelRibWidth/2     , TPCG_WheelR1 + TPCG_WheelRibWidth/2);
!!!!yf  WheelRib->AddVolume(wheelRib);
!!!!yf  TpcSectorAndWheel->AddNode(wheelRib,2,gGeoIdentity);

Attribute TWMR seen=1  colo=kYellow
Material ALUMINIUM
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
endBlock        "end TWMR"
*
*------------------------------------------------------------------------------
*
block TRDO  TpcRDOAssembly

Attribute TRDO seen=1  colo=kYellow
      SHAPE  PGON    Phi1=-15  Dphi=30  Nz=2 Npdv=1,
        zi ={zzzRDO(1),zzzRDO(2)},
        Rmn={rmnRDO(1),rmnRDO(2)},
        Rmx={rmxRDO(1),rmxRDO(2)}
        Attribute TRDO  seen=0 

!// RDOs and their cooling
!//  TGeoVolume *coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), TPCG_RDOCoolingdY, TPCG_RDOCoolingdX, TPCG_RDOCoolingdZ, +15, 0, 0);
!//  TpcRDO->AddNode(coolingTube,1,new TGeoCombiTrans(x, y, TPCG_zRDO - 2*dy,GetRot("YZXZ")));
  mySha = 'PARA';
  myPar = {TPCG_RDOCoolingdY, TPCG_RDOCoolingdX, TPCG_RDOCoolingdZ, 15};
  x = TPCG_WheelR2 - TPCG_RDOCoolingdX;
  y = x*tan15 - TPCG_RDOCoolingdY ;
  z = TPCG_zRDO - 2*dy;
  Create And Position TCOO x=x y=y z=z ORT=YX-Z
*===================================================

!//  coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), TPCG_RDOCoolingdY, TPCG_RDOCoolingdX,TPCG_RDOCoolingdZ, -15, 0, 0);
!//  TpcRDO->AddNode(coolingTube,2,new TGeoCombiTrans(x,-y, TPCG_zRDO - 2*dy,GetRot("YZXZ")));
  mySha = 'PARA';
  myPar = {TPCG_RDOCoolingdY, TPCG_RDOCoolingdX, TPCG_RDOCoolingdZ, -15};
  x = TPCG_WheelR2 - TPCG_RDOCoolingdX;
  y = x*tan15 - TPCG_RDOCoolingdY ;
  z = TPCG_zRDO - 2*dy;
  Create And Position TCOO x=x y=-y z=z ORT=YX-Z
*===================================================


  RCoolingTube = TPCG_WheelR2 - TPCG_heigTube - 2*TPCG_dxRDO;
  do iCoo = 0,7
  {
    dz = TPCG_heigTube*cm/2;
    dy = TPCG_widTube*cm/2;
    z  = RCoolingTube;
    dx1 = (z-dz)*tan15 - 2*TPCG_RDOCoolingdY;
    dx2 = (z+dz)*tan15 - 2*TPCG_RDOCoolingdY;


!//    TGeoVolume *coolingTube = gGeoManager->MakeTrd1("CoolingTube", GetMed("TPCE_Water_Pipe"), dx1, dx2, dy, dz);
!//    TpcRDO->AddNode(coolingTube,iCoo+3, new TGeoCombiTrans(z, 0, TPCG_zRDO - 2*dy, GetRot("90XD")));
    mySha = 'TRD1';
    myPar = {dx1, dx2, dy, dz};
    myPar(11) = z; myPar(13) = TPCG_zRDO - 2*dy;
    Create And Position TCOO x=myPar(11) y=0 z=myPar(13) ORT=YZX
!//     write(*,*) '###TRDO.TCOO myPar(1-4)=', myPar(1),myPar(2),myPar(3),myPar(4);
!//     write(*,*) '###TRDO.TCOO myPar(11,13)=', myPar(11),myPar(13);

    if (iCoo. ne. 0 .and. iCoo .ne. 6) {
!//      TpcRDO->AddNode(RDOCard, iRDOCard++, new TGeoTranslation(z+dz+TPCG_dxRDO, 0, TPCG_zRDO - TPCG_dzRDO));
       myPar(11) = z+dz+TPCG_dxRDO; myPar(13) =TPCG_zRDO-TPCG_dzRDO;
       Create And Position TRDC x=myPar(11) y=0 z=myPar(13);
!//     write(*,*) '###TRDO.TRDC myPar(11,13)=', myPar(11),myPar(13);
    }
    RCoolingTube -= dRDOCooling(iCoo);
  }

endBlock        "end TRDO"
*
*------------------------------------------------------------------------------
*
block TBRW  WheelRibBox

!!!!yf  TGeoVolume *WheelRibBox = gGeoManager->MakeBox("WheelRibBox",GetMed("TPCE_ALUMINIUM"), TPCG_WheelBoxDy/2, TPCG_WheelBoxDx/2, TPCG_WheelTHK/2);
!!!!yf  WheelRibBox->SetLineColor(kBlue);
!!!!yf  TpcSectorAndWheel->AddNodeOverlap(WheelRibBox, 1, new TGeoTranslation(TPCG_WheelR1, 0., zWheel1+TPCG_WheelTHK/2));
Attribute TBRW seen=1  colo=kViolet
Material ALUMINIUM
SHAPE  BOX dX=TPCG_WheelBoxDy/2 dY=TPCG_WheelBoxDx/2 dZ=TPCG_WheelTHK/2
endBlock        "end TBRW"
*
*------------------------------------------------------------------------------
*
!//    wheelRib = gGeoManager->MakePara("TpcWheelRib", GetMed("TPCE_ALUMINIUM"), dy, dx, dz,-(1-2*inOut)*15, 0, 0);
block TWRB TpcWheelRib
Attribute TWRB seen=1  colo=kYellow
Material ALUMINIUM
SHAPE PARA dX=myDx dY=myDy dz=myDz Alph=myAlph
endBlock        "end TWRB"
*
*------------------------------------------------------------------------------
*
block TCOO      CoolingTube

Attribute TCOO seen=1  colo=kRed
Material Water_Pipe
!// assert( mySha .eq. 'PARA'  .or. mySha .eq. 'TRD1');
 if (mySha .eq. 'PARA') then
   SHAPE PARA dX=myPar(1) dY=myPar(2) dz=myPar(3) Alph=myPar(4)
 endif
 if (mySha .eq. 'TRD1') then
   SHAPE TRD1 dX1=myPar(1) dX2=myPar(2) dY=myPar(3) dZ=myPar(4)
 endif

endBlock        "end TCOO"
*
*------------------------------------------------------------------------------
*
block TCAB      Cables
!//TGeoVolume *cables = gGeoManager->MakePara("Cables", GetMed("Cables"), dy, dx, dz,-15*(1 - 2*iCab), 0, 0);
!//TpcSectorAndWheel->AddNode(cables,iCab+1, new TGeoCombiTrans( x, y, z,GetRot("YZXZ")));

Attribute TCAB seen=1  colo=kGreen
Material Cables
   SHAPE PARA dX=myPar(1) dY=myPar(2) dz=myPar(3) Alph=myPar(4)
endBlock        "end TCAB"

*
*------------------------------------------------------------------------------
*
block TRDC      RDOCard
!//  TGeoVolume *RDOCard = gGeoManager->MakeBox("RDOCard", GetMed("TPCE_G10"), TPCG_dxRDO, TPCG_dyRDO, TPCG_dzRDO);
        Attribute TRDC seen=1  colo=kBlue
!        Material G10 
*   Effective G10 + electonics + connectors
*   the following is taken
      Component  Si  A=28.08  Z=14  W=0.6*1*28./60.
      Component  O   A=16     Z=8   W=0.6*2*16./60.
      Component  C   A=12     Z=6   W=0.4*8*12./174.
      Component  H   A=1      Z=1   W=0.4*14*1./174.
      Component  O   A=16     Z=8   W=0.4*4*16./174.
      Mixture G10RDO  Dens=1.7/2.41
      material  G10RDO

        SHAPE PARA dX=TPCG_dxRDO dY=TPCG_dyRDO dZ=TPCG_dzRDO
endBlock        "end TRDC"
*
*------------------------------------------------------------------------------
*
block TRIB      Tpc Ribs
!//     TGeoVolume *trib = gGeoManager->MakePara("TRIB", GetMed("TPCE_ALUMINIUM"), dx, dy, dz,-(1-2*i)*alpha, 0, 0);
        Attribute TRib seen=1  colo=kRed
        Material ALUMINIUM

!//        do jTRIB=1,3;
!//       assert(myPar(jTRIB).gt.0);
!//     enddo;

        if (mySha == 'PARA') {
          SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3) Alph=myPar(4)
        }
        if (mySha == 'TRD1') {
          SHAPE TRD1 dX1=myPar(1) dX2=myPar(2) dY=myPar(3) dZ=myPar(4)
        }
        if (mySha == 'BOX ') {
          SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3)
        }
endBlock        "end TRIB"
*
*------------------------------------------------------------------------------
*
block TWIR      WireMount
!//TGeoVolume *wireMount = gGeoManager->MakePara("WireMount", GetMed("TPCE_G10"), dxW, dy, dzW,-(1-2*iRib)*alpha,0,0)
        Attribute TWIR seen=1  colo=kYellow
        Material G10
        SHAPE PARA dX=myPar(1) dY=myPar(2) dZ=myPar(3) Alph=myPar(4)

endBlock        "end TWIR"
*
*------------------------------------------------------------------------------
*
block THOL the Sector holes for FEE
!//    TGeoVolume *thole = gGeoManager->MakeBox("THOLE", GetMed("TPCE_STANDARD"), dx, dy, dz);
        Attribute THOL seen=1  colo=kGreen
        Material Air;
!//        assert(myPar(1).gt.0);
!//        assert(myPar(2).gt.0);
!//        assert(myPar(3).gt.0);
        SHAPE BOX dX=myPar(1) dY=myPar(2) dZ=myPar(3)

        Create and Position FEES


endBlock        "end THOL"
*
*------------------------------------------------------------------------------
*
Block FEES  TFEESocket
!//  TGeoVolume *TFEESocket = gGeoManager->MakeBox("TFEESocket", GetMed("TPCE_G10"), 6.2/20, 57./20, 5.8/20)
!//  TFEESocket->SetTitle("Socket soldered to sector to connect FEE")
      material  G10
      Attribute FEES      seen=1  colo=kBlue
      SHAPE     BOX    dX=6.2/20, dY=57./20, dZ=5.8/20
endBlock        "end FEES"
*
******************************************************************************
*                       section one - sensitive gas                          *
******************************************************************************
*
Block TPGV is the Gas Volume placed in TPC
*
      Attribute TPGV      seen=1  colo=kRed
      Material P10
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpgvLeng/2
!//VP      Create    TPSS
     do iSecAng = 15,360-15,30
       Create and Position TPSS            alphaz=iSecAng kOnly='MANY'
     endDo
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block  TPSS is a division of gas volume corresponding to a supersectors
      attribute TPSS  seen=1  colo=kBlue
!//      shape Division  NDIV=12  IAXIS=2
      SHAPE TUBS Phi1=-15 Phi2=15

*
      ag_ncopy = 1;
      do i_sec=1,2
	Use    TPRS  sec=i_sec;
!//	write(*,*) ' TPSS : sec = ',10*TPCG_TpadConfig+i_sec,' super =',tprs_super
         Use    TECW  sec=i_sec;
         zWheel1  = TPCG_LengthW/2 - TPCG_WheelTHK
         zBeg = TPCG_MembTHK/2
         zEnd = TPCG_zGroundGrid + TPRS_dAnode*2;
         zDed = TPCG_zGatingGrid - TPCG_DeadZone;
	 zPmt = TPCG_zGatingGrid;
         dx=tprs_width/2;
                             dz = (zEnd - zBeg)/2; z= (zEnd + zBeg)/2 -tpgvz;
	                     dz1= TPCG_DeadZone/2; z1=TPCG_zGatingGrid  - dz1 -dz;
!//        write(*,*) 'zBeg,zDed,Zpmt,zEnd,dz1,z1 = ',zBeg,zDed,Zpmt,zEnd,dz1,z1;
*        position within supersector (this assumes rectangular padrows)
           do i_row = 1,nint(tprs_nRow)
              if ((nint(tprs_super)==3 | i_row==1)) then
                 dy=tprs_npads(i_row)*tprs_pitch/2;
                 x=tprs_Rpads(i_row)-tprs_width;
                 Create and Position TPAD  x=x z=z dx=dx dy=dy dz=dz
!//                 write(*,*) 'TPAD.A Sec=',i_sec,AG_NCOPY,' Z1=',z-dz+tpgvz,' Z2=',z+dz+tpgvz;
              endif
                 dy=tprs_npads(i_row)*tprs_pitch/2;
                 x=tprs_Rpads(i_row);
                 create and position TPAD  x=x z=z dx=dx dy=dy dz=dz
!//                  write(*,*) 'TPAD.B Sec=',i_sec,AG_NCOPY,' Z1=',z-dz+tpgvz,' Z2=',z+dz+tpgvz;
              if ((nint(tprs_super)==3 | i_row==nint(tprs_nRow)))  then
                 x=tprs_Rpads(i_row)+tprs_width
                 dy=tprs_npads(i_row)*tprs_pitch/2;
                 Create and Position TPAD  x=x z=z dx=dx dy=dy dz=dz
!//                  write(*,*) 'TPAD.C Sec=',i_sec,AG_NCOPY,' Z1=',z-dz+tpgvz,' Z2=',z+dz+tpgvz;
              endif
           enddo
      enddo
Endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
block TPAD is a real padrow with dimensions defined at positioning time
* We want to use PAI model in padrows to simulate energy loss more precisely.
* (see GEANT PHYS334, Allisson and Cobb, Ann.Rev.Nucl.Part.Sci.30(1980),253).
* (    best formalism in Grishin,Ermilova,Kotelnikov, NIM A307(1991),273).
* To switch it ON only in this volume we introduce P10 as a different MATERIAL,
* since ISTRA flag is kept in medium, but all tables are stored in the material
* According to AGI rules for this we need A parameter. Lets use ISVOL=1 (!)
* (- this eliminates a need for a separate medium definition ?)
*
      attribute TPAD seen=1 colo=2
      material p10
      material sensitive_gas  ISVOL=1  stemax=2.5*tprs_width
      SHAPE    BOX   dx=0   dy=0   dz=0
      Call     GSTPAR(ag_imed,'CUTGAM',1e-4)
      Call     GSTPAR(ag_imed,'CUTELE',1e-4)
      Call     GSTPAR(ag_imed,'DCUTE', 1e-4)
      Call     GSTPAR(ag_imed,'DCUTM', 1e-4)
*     Call     GSTPAR(ag_imed,'STRA',1.)
      Call     GSTPAR(ag_imed,'LOSS',1.)
*
*     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    TPAD   Z:.0005:S  Y:.0005:  X:.0005:   cx:10: cy:10: cz:10:,
                     LPtot:18:(-3,2)      Sleng:.1:(0,800),
                     ToF:16:(0,32.e-6)     LGAM:16:(-2,2),
                     Step:11:(0,10)       USER:21:(-.01,.01)
#if 0
      create and position TDEA  dx=dx dy=dy z=z1 dz=dz1
!//	write(*,*) 	'TDEA, dx,dy,dz1,z1=',dx,dy,dz1,z1
#endif
endblock
#if 0
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
block TDEA is a dead region in pad row
      attribute TDEA seen=1 colo=4
      material p10
      SHAPE    BOX dx=0 dy=0 dz=0 
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
      end
