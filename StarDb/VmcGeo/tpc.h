// $Id: tpc.h,v 1.14 2011/02/11 16:16:36 fisyak Exp $
// $Log: tpc.h,v $
// Revision 1.14  2011/02/11 16:16:36  fisyak
// Fix for prompt hits
//
// Revision 1.13  2009/04/09 21:49:45  fisyak
// swap  (x,y,z) = > ( x,-y, z) for sensitive detectors
//
// Revision 1.12  2009/01/16 16:00:29  fisyak
// suppress print outs
//
// Revision 1.11  2008/12/01 21:51:52  fisyak
// Add Cables to Tpc, add more indexes
//
// Revision 1.10  2008/11/25 00:07:44  fisyak
// use new Tpc Geometry
//
// Revision 1.5  2008/11/25 00:06:37  fisyak
// Fix overlaps
//
// Revision 1.4  2008/11/20 17:21:02  fisyak
// Clean up
//
// Revision 1.3  2008/11/20 00:10:39  fisyak
// Add Wire Mount
//
// Revision 1.2  2008/11/18 00:06:12  fisyak
// Finish with Wheel
//
// Revision 1.1  2008/11/17 14:14:46  fisyak
// new Tpc Geometry
//
// Revision 1.8  2008/10/21 14:57:29  fisyak
// add missing gaps in wheel
//
// Revision 1.7  2008/10/20 16:14:37  fisyak
// Clean up
//
// Revision 1.6  2008/10/20 15:40:59  fisyak
// Freeze TIFC and TOFC
//
// Revision 1.5  2008/09/03 20:44:48  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
// Revision 1.4  2008/08/27 21:48:17  fisyak
//
// TPC 
#ifndef tpcConfig
#define tpcConfig 3
#endif
//#ifdef tpcConfig
#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "Rotations.h"
#include "Material.h"
#include "Media.h"
#include "CreateGeometry.h"
#include "TGeoMatrix.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPara.h"
#include "TGeoCompositeShape.h"
#include "TGeoVolume.h"
using namespace std;
//________________________________________________________________________________
TGeoVolume *tpc() {
  Double_t inch = 2.54; 
  Double_t cm   = 1.00;
  //  Double_t lb   = 0.45359; // kG
  Double_t cos15 = TMath::Cos(15./180*TMath::Pi());
  Double_t tan15 = TMath::Tan(TMath::Pi()*15./180);
  // TPC Parameters
  struct TPCG_t {//   TPC basic dimensions
    Int_t     version;    // current version
    Double_t  Rmin;       // inner radius of Tpc envelope
    Double_t  Rmax;       // outer radius of Tpc envelope
    Double_t  RminIFC;    // inner radius TPC IFC
    Double_t  LengthT;    // TPC full length up to EEMC
    Double_t  Length;     // TPC + electonics length
    Double_t  LengthW;    // TPC length including Wheel
    Double_t  LengthV;    // TPC gas volume length
    Double_t  WheelIR;    // support wheel inner radius
    Double_t  WheelR0;    // Distance from IP of end of inner cylindrical part
    Double_t  WheelR1;    // Distance from IP of middle Rib
    Double_t  WheelR2;    // Distance from IP of upper Rib
    Double_t  WheelTotalRibWidth; //  total rib width
    Double_t  WheelRibWidth; //  rib plate width
    Double_t  WheelRibHeight;//  rib plate height
    Double_t  WheelBoxDx;
    Double_t  WheelBoxDy;
    Double_t  WheelOR1;   // support wheel outer radius @ TOFC
    Double_t  WheelOR;    // support wheel outer radius
    Double_t  WheelTHK1;
    Double_t  WheelTHK;   // support wheel length
    Double_t  SenGasOR;   // TPC sensitive gas outer radius
    Double_t  MembTHK;    // Central membrane thickness
    Double_t  tocsDR;     // outer copper thickness
    Double_t  tokaDR;     // outer kapton thickness
    Double_t  tonxDR;     // outer nomex thickness
    Double_t  toadDR;     // outer adhesive thickness
    Double_t  toigDR;     // outer isolating gas thickness
    Double_t  toalDR;     // outer aluminum thickness
    Double_t  tohaDR;     // outer HoneyUcomb Al thickness  

    Double_t  tiadDR;     // inner adhesive layer thickness
    Double_t  tinxDR;     // inner nomex structure thickness
    Double_t  tikaDR;     // inner Kapton layer thickness
    Double_t  tialDR;     // inner aluminum layer thickness
    Double_t  tifcRF;     // outer radius of IFC flange 
    Double_t  tifcDRT;    // tolerance between flange and wheel
  };
  TPCG_t TPCG = {//   TPC basic dimensions
    tpcConfig        , // version    => current version
    46.107   , // Rmin       => TPC envelope inner radius
    207.750  , // Rmax       => TPC envelope outer radius
    46.6     , // RminIFC    => inner radius TPC IFC  : A.Lebedev measurement 10/16/08
    2*259.685, // LengthT    => TPC full length up to fron of EEMC
    2*259.685, // Length     => TPC full length including RDOs
    2*229.71 , // LengthW    => TPC length including Wheel
    2*209.99 , // LengthV    => TPC gas volume length
    38.620*inch/2, // 49.60, WheelIR    => support wheel inner radius
    21.500*inch, // WheelR0 => Distance from IP of end of inner cylindrical part
    47.867*inch, // WheelR1 => Distance from IP of middle Rib
    76.093*inch, // WheelR2 => Distance from IP of upper Rib
    7.6,       // WheelTotalRibWidth => total rib width (A.Lebedev measurement)
    1.9,       // WheelRibWidth      => rib plate width (A.Lebedev measurement)
    1.3,       // WheelRibHeight     => rib plate height (A.Lebedev measurement)
    4.7,       // WheelBoxDx (A.Lebedev measurement)
    7.6,       // WheelBoxDy (A.Lebedev measurement)
    201.0    , // WheelOR1   => @ TOFC
    206.75   , // WheelOR    => support wheel outer radius
    5.72     , // WheelTHK1
    11.43    , // WheelTHK   => support wheel length
    200      , // SenGasOR   => TPC sensitive gas outer radius
    .00762   , // MembTHK    => Central membrane thickness

    0.013    , // tocsDR     => outer copper thickness:  NIM A499 (2003), 659-678, Table 2
    0.015    , // tokaDR     => outer kapton thickness
    0.953    , // tonxDR     => outer nomex thickness
    0.05     , // toadDR     => outer adhesive thickness

    5.70     , // toigDR     => outer isolating gas thickness
    0.40     , // toalDR     => outer aluminum thickness (for both layers) 
    0.60     , // tohaDR     => outer Honeycomb Al thickness  

    0.080    , // tiadDR     => inner adhesive layer thickness:  NIM A499 (2003), 659-678, Table 2
    1.270    , // tinxDR     => inner nomex structure thickness
    0.015    , // tikaDR     => inner Kapton layer thickness
    0.004    , // tialDR     => inner aluminum layer thickness
    51.7     , // tifcRF     => outer radius of IFC flange
    0.1        // tifcDRT    => tolerance between flange and wheel
  };

  struct TPRS_t {
    Int_t         sec;  // sector number: 1 for inner, 2 for outer
    Int_t         nRow; // number of padrows in the sector
    Double_t     pitch; // tpc padrow pitch width
    Double_t     width; // tpc padrow thickness
    Double_t     dAnode;// distance to anode wire from pad plane
    Int_t       *Npads; // number of pads in row
    Double_t    *Rpads; // tpc padrow radii
  };
  TPRS_t TPRS[2] = { //  sector of padrows
    {1,            // sec    =sector number: 1 for inner, 2 for outer
     13,           // nRow   =number of padrows in the sector
     0.335,        // pitch  =tpc padrow pitch width
     1.15,         // width  =tpc padrow thickness
     0.2,          // dAnode =distance to anode wire from pad plane
     0,            // Npads  =number of pads in row
     0             // Rpads  =tpc padrow radii
    },
    {2,            // sec    = sector number: 1 for inner, 2 for outer
     32,           // nRow   = number of padrows in outer sector
     0.67,         // pitch  = outer tpc padrow pitch width
     1.95,         // width  = outer tpc padrow thickness
     0.4,          // dAnode =distance to anode wire from pad plane
     0,            // Npads  =number of pads in row
     0}            // Rpads  =tpc padrow radii
  };
  Int_t NpadsI[13] = { // Npads  =number of pads in row
    88,   96, 104, 112, 118, 126, 134, 142, 150, 
    158, 166, 174, 182};
  Double_t RpadsI[13] = {// Rpads  =tpc padrow radii
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8, 
    104.0,109.2,114.4,119.6};
  Int_t NpadsO[32] = {// Npads  =number of pads in row
    98, 100, 102, 104, 106, 106, 108, 110, 112, 
    112, 114, 116, 118, 120, 122, 122, 124, 126, 
    128, 128, 130, 132, 134, 136, 138, 138, 140, 
    142, 144, 144, 144, 144 };
  Double_t RpadsO[32] = {// Rpads  =tpc padrow radii
    127.195, 129.195, 131.195, 133.195, 135.195, 
    137.195, 139.195, 141.195, 143.195, 145.195, 
    147.195, 149.195, 151.195, 153.195, 155.195, 
    157.195, 159.195, 161.195, 163.195, 165.195, 
    167.195, 169.195, 171.195, 173.195, 175.195, 
    177.195, 179.195, 181.195, 183.195, 185.195, 
    187.195, 189.195 };
  TPRS[0].Npads = &NpadsI[0];
  TPRS[0].Rpads = &RpadsI[0];
  TPRS[1].Npads = &NpadsO[0];
  TPRS[1].Rpads = &RpadsO[0];
  struct TECW_t {// EC trapezoid and support Wheel
    Int_t           sec; // sector number: 1 for inner, 2 for outer
    Double_t    GapWidI; // air in support wheel - inner width
    Double_t    GapWidO; // air in support wheel - outer width
    Double_t    GapHeit; // air in support wheel - height (dr)
    Double_t    GapRad;  // air in support wheel - center radius
    Double_t    inwidth; // sector width at inner radius 
    Double_t    ouwidth; // sector width at outer radius
    Double_t    widthLip;// width of lip 
    Double_t    noribs;  // no. of ribs
    Double_t    zStepRib;
    Double_t    widthRib; 
    Double_t    Height;  // sector radial Height
    Double_t    Thick;   // sector thickness
    Double_t    ThickAl; // Thick - air gap
    Double_t    rMin;    // Minimum distance to beam line
    Double_t    Rcenter; // sector center radius (set by precision holes)
  };
  TECW_t TECW[2] = {// EC trapezoid and support Wheel
    {1,          // sec      => sector number: 1 for inner, 2 for outer
     2.* 10.91,  // GapWidI  => air in support wheel - inner width
     2.* 27.56,  // GapWidO  => air in support wheel - outer width
     62.15,      // GapHeit  => air in support wheel - Height (dr)
     87.0,       // GapRad   => air in support wheel - center radius
     10.829*inch,//2.* 13.75,  // inwidth  => sector width at inner radius 
     25.498*inch,//2.* 32.38,  // ouwidth  => sector width at outer radius
     ((10.829-9.852)+(25.498-24.521))/4*inch, // 1.24079  widthLip =>  width of lip 
     11,          // noribs
     3.150*inch,        // zStepRib
     1.47*TMath::Cos(15./180*TMath::Pi())*inch,// widthRib => side rib width
     27.373*inch,// Drawing 24A3685B; 27.77*inch from drawing 24A0325, => 69.527 => (Wire Plane)  69.52,  Height   => sector radial Height
     3.300*inch, // 24A3685B  3.219*inch, // Thick    => sector thickness
     0.375*inch, // = 9.525 mm, ThickAl  => Thick - air gap
     51.905,     // rMin     => Minimum distance to beam line (wire plane)
     86.669      // Rcenter  => sector center radius (set by precision holes)
    },             
    {2,          // sec      => sector number: 1 for inner, 2 for outer
     2.* 28.92,  // GapWidI  => air in support wheel - inner width
     2.* 46.74,  // GapWidO  => air in support wheel - outer width
     65.0,       // GapHeit  => air in support wheel - Height (dr)
     158.0,      // aGapRad   => ir in support wheel - radius
     25.561*inch,//64.92,      // inwidth  => sector width at inner radius 
     40.649*inch,//103.25,     // ouwidth  => sector width at outer radius 
     ((25.561-24.583)+(40.649-39.671))/4*inch, // = 1.242060 widthLip =>  width of lip
     14,                 // noribs
     3.150*inch,        // zStepRib
     1.47*TMath::Cos(15./180*TMath::Pi())*inch,// widthRib = > side rib width
     28.155*inch,// 71.514 (Wire Plane) => 71.51,      // Height   => sector radial Height
     3.140*inch, // 24A3925G thick    => sector thickness
     0.375*inch, // = 9.525 mm; ThickAl  => Thick - air gap
     121.732,    // rMin     => Minimum distance to beam line (wire plane)
     157.488     // Rcenter  => sector center radius (set by precision holes)
    }              
  };
  /*
   *  TPC basic dimensions are the full system size, the gas volume length
   *  and inner radius are derived from them and from material thicknesses.
   *  The outer gas radius is also an input ( as this is used for winding ),
   *  but the remaining clearance is checked to be positive (PN, 16 Mar 96).
   */
  /*
   * layer names mnemonic:
   *-----------------------------------------------------------------------
   * letter 1-6 :   t  :     i/o     :  cs/ka/nx/al/fc/fs  :     IR/OR    :
   *-----------------------------------------------------------------------
   *   meaning  :  TPC : inner/outer :    materials as     :  inner/outer :
   *            :      :     cage    : Copper shild,Kapton :    radius    :
   *            :      :             : Nomex,Aluminum  OR  :    of the    :
   *            :      :             : field cage and      :    layer     :
   *            :      :             : field cage support  :              :
   *-----------------------------------------------------------------------
   */
  // calculate radii of outer finest structures 
  Double_t    tocsIR = TPCG.SenGasOR;     Double_t    tocsOR = tocsIR + TPCG.tocsDR;
  Double_t    tokaIR = tocsOR;            Double_t    tokaOR = tokaIR + TPCG.tokaDR;
  Double_t    tonxIR = tokaOR;            Double_t    tonxOR = tonxIR + TPCG.tonxDR;
  Double_t    toadIR = tonxOR;            Double_t    toadOR = toadIR + TPCG.toadDR;

  Double_t    toigIR = toadOR;            Double_t    toigOR = toigIR + TPCG.toigDR;
  Double_t    toalIR = toigOR;            Double_t    toalOR = toalIR + TPCG.toalDR/2.;
  Double_t    tohaIR = toalOR;            Double_t    tohaOR = tohaIR + TPCG.tohaDR;
  Double_t    toalIR2= tohaOR;            Double_t    toalOR2= toalIR2+ TPCG.toalDR/2.;

  Double_t    tofcIR = tocsIR;          Double_t    tofcOR = toalOR2;
#ifdef DEBUG
  Double_t    tofsIR = tocsIR;          Double_t    tofsOR = toadOR; // ?
  cout << "tocsIR = " << tocsIR << "\ttocsOR = " << tocsOR << endl;
  cout << "tokaIR = " << tokaIR << "\ttokaOR = " << tokaOR << endl;
  cout << "tonxIR = " << tonxIR << "\ttonxOR = " << tonxOR << endl;
  cout << "toadIR = " << toadIR << "\ttoadOR = " << toadOR << endl;
  cout << "toigIR = " << toigIR << "\ttoigOR = " << toigOR << endl;
  cout << "toalIR = " << toalIR << "\ttoalOR = " << toalOR << endl;
  cout << "tohaIR = " << tohaIR << "\ttohaOR = " << tohaOR << endl;
  cout << "toalIR2 = " << toalIR2 << "\ttoalOR2 = " << toalOR2 << endl;
  cout << "tofcIR = " << tofcIR << "\ttofcOR = " << tofcOR << endl;
  cout << "tofsIR = " << tofsIR << "\ttofsOR = " << tofsOR << endl;
#endif
  // calculate radii of inner finest structures
  Double_t    tifcIR = TPCG.RminIFC;  // inner field cage inner radius
  Double_t    tialIR = tifcIR;          Double_t    tialOR = tialIR + TPCG.tialDR/2.;
  Double_t    tikaIR = tialOR;          Double_t    tikaOR = tikaIR + TPCG.tikaDR;
  Double_t    tinxIR = tikaOR;          Double_t    tinxOR = tinxIR + TPCG.tinxDR;
  Double_t    tiadIR = tinxOR;          Double_t    tiadOR = tiadIR + TPCG.tiadDR;
                                        Double_t    tifcOR = tiadOR + TPCG.tialDR/2.;
#ifdef DEBUG
  // calculate radii of inner finest structures
  cout << "tialDR = " << TPCG.tialDR << "\ttialIR = " << tialIR << "\ttialOR = " << tialOR << endl;
  cout << "tikaDR = " << TPCG.tikaDR << "\ttikaIR = " << tikaIR << "\ttikaOR = " << tikaOR << endl;
  cout << "tinxDR = " << TPCG.tinxDR << "\ttinxIR = " << tinxIR << "\ttinxOR = " << tinxOR << endl;
  cout << "tiadDR = " << TPCG.tiadDR << "\ttiadIR = " << tiadIR << "\ttiadOR = " << tiadOR << endl;
  cout                               << "\ttifcIR = " << tifcIR << "\ttifcOR = " << tifcOR << endl;
#endif
  //  derive radii of larger structures
  Double_t    tpgvIR = tifcOR;         // TPC gas inner radius
  
  //  make primitive control and basic printout
  Double_t del = TPCG.Rmax-tofcOR;
//  printf(" TPCEgeo : tpcConfig = %i\n",TPCG.version);
//  printf(" TPCEgeo: maximum  TPC  Radius is %10.4f  clearance is %8.4f\n",tofcOR,del);
  if (del<0) printf (" *** TPCEgeo ERROR : outer clearance negative %f\n ",del);
//  printf(" TPCEgeo: senset. gas inner radius %10.4f\n",tpgvIR);
//  printf(" TPCEgeo: sensitive gas length is %10.4f\n", tpgvLeng);
//  printf("*** Building the Underwood version of the TPC ***\n");
  /*
    tocsIR = 200            tocsOR = 200.013
    tokaIR = 200.013        tokaOR = 200.028
    tonxIR = 200.028        tonxOR = 201.028
    toadIR = 201.028        toadOR = 201.048
    toigIR = 201.048        toigOR = 206.748
    toalIR = 206.748        toalOR = 207.148
    tohaIR = 207.148        tohaOR = 207.748
    tifcIR = 46.107
    tiadIR = 46.107         tiadOR = 46.187 
    tinxIR = 46.187 	    tinxOR = 47.457 
    tikaIR = 47.457 	    tikaOR = 47.472 
    tialIR = 47.472 	    tialOR = 47.476 
    tofcIR = 200    	    tofcOR = 207.748
    tofsIR = 200    	    tofsOR = 201.048
                            tifcOR = 47.476
    tpgvIR = 47.476  */
  //  derive radii of larger structures
#ifdef DEBUG
  cout << "\ttifcOR = " << tifcOR << endl;
  cout << "\ttpgvIR = " << tpgvIR << endl;
#endif
  TGeoPcon   *pcon = 0;
  TGeoPgon   *pgon = 0;
  TGeoPara   *para = 0;
  //__________________________________________ Geomety ______________________________________
  TGeoVolume *TPCE = gGeoManager->MakeTube("TPCE",GetMed("TPCE_STANDARD"),TPCG.Rmin,TPCG.Rmax,TPCG.LengthT/2); 
  TPCE->SetTitle("the TPC envelope");
  TPCE->SetVisibility(0);
  TPCE->SetLineColor(kBlack);
  //________________________________________________________________________________
  TGeoVolume *TPCM = gGeoManager->MakeTube("TPCM",GetMed("TPCE_MYLAR"),tpgvIR,TPCG.SenGasOR,TPCG.MembTHK/2); 
  TPCM->SetTitle("the Central Membrane placed in TPC");
  TPCM->SetLineColor(kBlue);
  //  TPCM->SetVisibility(1);
  TPCE->AddNode(TPCM,1,gGeoIdentity);
  //  cout << "LengthT/2\t" << TPCG.LengthT/2 << "\tLength/2\t" << TPCG.Length/2 << "\tLengthW/2\t" << TPCG.LengthW/2 << endl;
  Double_t zWheel1  = TPCG.LengthW/2 - TPCG.WheelTHK; // cout << "zWheel1\t" << zWheel1 << endl;
  Double_t zTOFCend = TPCG.LengthW/2 - TPCG.WheelTHK1;// cout << "zTOFCend\t" << zTOFCend << endl; // end of TOFC
  Double_t zTIFCend = TPCG.LengthW/2 + 0.97; // cout << "zTIFCend\t" << zTIFCend << endl;         // end of TIFC + flange
  Double_t zTIFCFlangeBegin = zTIFCend - 1*inch;// cout << "zTIFCFlangeBegin\t" << zTIFCFlangeBegin << endl;
  //________________________________________________________________________________  
  // Mylar " should be close to Adhesive "
  //  TGeoVolume *TIFC = gGeoManager->MakeTube("TIFC",GetMed("TPCE_Adhesive"),tifcIR,tifcOR,tofcLeng/2); 
  TGeoVolume *TIFC = gGeoManager->MakePcon("TIFC",GetMed("TPCE_ALUMINIUM"),0,360,6);
  pcon = (TGeoPcon *)  TIFC->GetShape();
  pcon->DefineSection(0,        -zTIFCend, tifcIR, TPCG.tifcRF);
  pcon->DefineSection(1,-zTIFCFlangeBegin, tifcIR, TPCG.tifcRF);
  pcon->DefineSection(2,-zTIFCFlangeBegin, tifcIR, tifcOR);
  pcon->DefineSection(3, zTIFCFlangeBegin, tifcIR, tifcOR);
  pcon->DefineSection(4, zTIFCFlangeBegin, tifcIR, TPCG.tifcRF);
  pcon->DefineSection(5,         zTIFCend, tifcIR, TPCG.tifcRF);
  Double_t dz  = 0;
  Double_t dz2 = 0;
  dz = 221.162; 
  TIFC->SetTitle("the Inner Field Cage placed in TPC");
  //  TIFC->SetVisibility(1);
  TIFC->SetLineColor(kBlue);
  TPCE->AddNode(TIFC,1,gGeoIdentity);
#if 0 /* Mother is Al made */
  TGeoVolume *TIAL = gGeoManager->MakeTube("TIAL",GetMed("TPCE_ALUMINIUM"),tialIR,tialOR,dz); 
  TIAL->SetTitle("the inner Aluminum cylinder");
  //  TIAL->SetVisibility(1);
  TIAL->SetLineColor(kBlue);
  TIFC->AddNode(TIAL,1,gGeoIdentity);
#endif
  TGeoVolume *TIKA = gGeoManager->MakeTube("TIKA",GetMed("TPCE_MYLAR"),tikaIR,tikaOR,dz); 
  TIKA->SetTitle("the kapton film of the inner field cage");
  TIKA->SetLineColor(kOrange);
  //  TIKA->SetVisibility(1);
  TIFC->AddNode(TIKA,1,gGeoIdentity);

  TGeoVolume *TINX = gGeoManager->MakeTube("TINX",GetMed("TPCE_NOMEX"),tinxIR,tinxOR,dz); 
  TINX->SetTitle("the inner nomex structure");
  TINX->SetLineColor(kYellow);
  //  TINX->SetVisibility(1);
  TIFC->AddNode(TINX,1,gGeoIdentity);
  
  TGeoVolume *TIAD = gGeoManager->MakeTube("TIAD",GetMed("TPCE_Adhesive"),tiadIR,tiadOR,dz); 
  TIAD->SetTitle("the inner adhesive structure");
  TIAD->SetLineColor(kYellow);
  //  TIAD->SetVisibility(1);
  TIFC->AddNode(TIAD,1,gGeoIdentity);
  //________________________________________________________________________________  
  //  TGeoVolume *TOFC = gGeoManager->MakeTube("TOFC",GetMed("TPCE_NITROGEN_GAS"),TPCG.SenGasOR,tofcOR,tofcLeng/2); 
  TGeoVolume *TOFC = gGeoManager->MakePcon("TOFC",GetMed("TPCE_ALUMINIUM"),0,360,6);
  pcon = (TGeoPcon *)  TOFC->GetShape();
  pcon->DefineSection(0,-zTOFCend, TPCG.WheelOR1, TPCG.WheelOR);
  pcon->DefineSection(1, -zWheel1, TPCG.WheelOR1, TPCG.WheelOR);
  pcon->DefineSection(2, -zWheel1,        tofcIR,       tofcOR);
  pcon->DefineSection(3,  zWheel1,        tofcIR,       tofcOR);
  pcon->DefineSection(4,  zWheel1, TPCG.WheelOR1, TPCG.WheelOR);
  pcon->DefineSection(5, zTOFCend, TPCG.WheelOR1, TPCG.WheelOR);
  TOFC->SetTitle("outer field cage and Gas Containment Vessel");
  TOFC->SetLineColor(kBlue);
  //  TOFC->SetVisibility(1);
  TPCE->AddNode(TOFC,1,gGeoIdentity);
  // Outer Field Cage = FS (Cu) + KA (Kapton==Mylar) + NX (Nomex) + AD (Adhesive)

  dz = 211.9;
  TGeoVolume *TOFS = gGeoManager->MakeTube("TOFS",GetMed("TPCE_COPPER"),tocsIR,tocsOR,dz); 
  TOFS->SetTitle("the Outer Field Cage copper structure");
  TOFS->SetLineColor(kBlue);
  //  TOFS->SetVisibility(1);
  TOFC->AddNode(TOFS,1,gGeoIdentity);
  
  TGeoVolume *TOKA = gGeoManager->MakeTube("TOKA",GetMed("TPCE_MYLAR"),tokaIR,tokaOR,dz); 
  TOKA->SetTitle("KAPTON layer");
  TOKA->SetLineColor(kGreen);
  //  TOKA->SetVisibility(1);
  TOFC->AddNode(TOKA,1,gGeoIdentity);
  
  dz2 = 208.02;
  //  TGeoVolume *TONX = gGeoManager->MakeTube("TONX",GetMed("TPCE_NOMEX"),tonxIR,tonxOR,dz2); 
  TGeoVolume *TONX = gGeoManager->MakePcon("TONX",GetMed("TPCE_NOMEX"),0,360,6);
  pcon = (TGeoPcon *)  TONX->GetShape();
  pcon->DefineSection(0,-dz , tonxOR-0.2,tonxOR);
  pcon->DefineSection(1,-dz2, tonxOR-0.2,tonxOR);
  pcon->DefineSection(2,-dz2, tonxIR,tonxOR);
  pcon->DefineSection(3, dz2, tonxIR,tonxOR);
  pcon->DefineSection(4, dz2, tonxOR-0.2,tonxOR);
  pcon->DefineSection(5, dz , tonxOR-0.2,tonxOR);
  TONX->SetTitle("Nomex support");
  TONX->SetLineColor(kBlack);
  //  TONX->SetVisibility(1);
  TOFC->AddNode(TONX,1,gGeoIdentity);
  
  TGeoVolume *TOAD = gGeoManager->MakeTube("TOAD",GetMed("TPCE_MYLAR"),toadIR,toadOR,dz); 
  TOAD->SetTitle("Adhesive layer");
  TOAD->SetLineColor(kViolet);
  //  TOAD->SetVisibility(1);
  TOFC->AddNode(TOAD,1,gGeoIdentity); 

  // Insulating Gas (Nitrogen) 

  TGeoVolume *TOIG = gGeoManager->MakeTube("TOIG",GetMed("TPCE_NITROGEN_GAS"),toigIR,toigOR,dz); 
  TOIG->SetTitle("Insulating Gas");
  TOIG->SetLineColor(kBlack);
  //  TOIG->SetVisibility(1);
  TOFC->AddNode(TOIG,1,gGeoIdentity);

  // Gas Containment Vessel (Al) + HA
 
  TGeoVolume *TOHA = gGeoManager->MakeTube("TOHA",GetMed("TPCE_AL_HONEYCOMB"),tohaIR,tohaOR,dz2); 
  TOHA->SetTitle("Honeycomb/Adhesive mixture");
  TOHA->SetLineColor(kViolet);
  //  TOHA->SetVisibility(1);
  TOFC->AddNode(TOHA,1,gGeoIdentity);
#if 0
  Double_t dz3 = zTOFCend - 3.65*inch;
  Double_t dz4 = dz3 + 0.8*inch;
  TGeoVolume *TOFCRing = gGeoManager->MakePcon("TOFCRing",GetMed("PIPE_IRON"),0,360,8);
  pcon = (TGeoPcon *)  TOFCRing->GetShape();
  pcon->DefineSection(0,       dz, toigIR,toigOR);
  pcon->DefineSection(1,      dz3, toigIR,toigOR);
  pcon->DefineSection(2,      dz3, tofcIR,tofcOR);
  pcon->DefineSection(3,      dz4, tofcIR,tofcOR);
  pcon->DefineSection(4,      dz4, TPCG.WheelOR1+1*inch, TPCG.WheelOR-1*inch);
  pcon->DefineSection(5,  zWheel1, TPCG.WheelOR1+1*inch, TPCG.WheelOR-1*inch);
  pcon->DefineSection(6,  zWheel1, TPCG.WheelOR1, TPCG.WheelOR);
  pcon->DefineSection(7, zTOFCend, TPCG.WheelOR1, TPCG.WheelOR);
  TOFC->AddNode(TOFCRing,1,gGeoIdentity);
  TOFC->AddNode(TOFCRing,2,GetRot("180X"));
#endif
  //________________________________________________________________________________  
  // TpcSectorWhole is Sector as Whole
  //________________________________________________________________________________  
  TGeoVolume *TpcSectorWhole = gGeoManager->MakePcon("TpcSectorWhole",GetMed("TPCE_STANDARD"),-15,30,8);
  pcon = (TGeoPcon *) TpcSectorWhole ->GetShape();
  pcon->DefineSection(0,  TPCG.MembTHK/2, tifcOR, tofcIR);                       // membrane 
  pcon->DefineSection(1,         zWheel1, tifcOR, tofcIR);        // gas volume + sector
  pcon->DefineSection(2,         zWheel1, tifcOR, TPCG.WheelOR1); // wheel begins
  pcon->DefineSection(3,        zTOFCend, tifcOR, TPCG.WheelOR1); 
  pcon->DefineSection(4,        zTOFCend, tifcOR, TPCG.WheelOR); // wheel with TOFC 
  pcon->DefineSection(5,zTIFCFlangeBegin, tifcOR, TPCG.WheelOR); // wheel with TIFC flange
  pcon->DefineSection(6,zTIFCFlangeBegin, TPCG.tifcRF+TPCG.tifcDRT, TPCG.WheelOR);
  pcon->DefineSection(7,  TPCG.LengthT/2, TPCG.tifcRF+TPCG.tifcDRT, TPCG.WheelOR);
  TpcSectorWhole->SetTitle("Tpc Sector as Whole");
  //  TpcSectorWhole->SetVisibility(1);
  TpcSectorWhole->SetLineColor(kBlack);
  Int_t   sec = 0;
  TString Rot;
  for (sec = 1; sec <= 24; sec++) {
    if (sec <= 12) Rot = Form("R%03i",(360 + 90 - 30* sec    )%360);
    else           Rot = Form("Y%03i",(      90 + 30*(sec-12))%360);
    TPCE->AddNode(TpcSectorWhole,sec, GetRot(Rot));
  }
  //________________________________________________________________________________  
  // Tpc Gas Volume  and  Sector + Wheel
  TGeoPgon *SectorItSelves[2] = {0,0};
  TGeoTranslation *SectorShifts[2] = {0,0};
  // Wheel is the TPC supporting endcap Wheel
  TGeoTranslation *WheelShifts[2] = {0,0};
  const Char_t *InnerOuter[2] = {"Inner","Outer"};
  Double_t x1, x2, y1, y2, x0, k, r1, r2, dr, z1, z2, xc;
  Double_t dx1, dx2, r, dx, dy;
  Double_t x, y, z;
  Double_t alpha = 0;
  Int_t i = 0;
  Int_t j = 0;
  Double_t dRSectorShift = 0; // Sector shift in radius to provide gap between sectors
  Double_t dRWheelShift = 0;
  Double_t PadPlaneThickness = 0.182; // 1.82 mm  Drawing 24A0314 & 24A0304
  Double_t WireMountHeight = 1.340*inch; // Drawing 24A0434
  Double_t WireMountWidth  = 5*0.130*inch; 

   // Sectors
  struct RIB_t { // all dimension in inches
    Double_t x;  // distance from top edge of sector to rib top
    Double_t dx; // rib height
    Double_t y;  // rib position in Y
    Double_t dy; // thickness of rib in Y direction
  };
#if 0
  Double_t dX2 = 2*(1.470 - 1.050);
  Double_t dX  = 1.050 - dX2/2;
#else
  Double_t dX2 = 1.470;
  Double_t dX  = 0;
#endif
  RIB_t RIBI[11] = { // Drawing 24A3685B height =  27.373
    { dX              ,              dX2,             0.,   0.}, // r for upper edge
    { 4.261           ,            0.375,             0.,   0.},
    {10.561           ,            0.375,             0.,   0.},
    {16.861           ,            0.375,             0.,   0.},
    {23.161           ,            0.375,             0.,   0.},
    {25.950  ,TECW[0].Height/inch-25.950,             0.,   0.},
    {4.261-1.070/2    ,            1.070,         -5.645,0.750},  // r for center
    {4.261-1.070/2    ,            1.070,          5.645,0.750},  // r for center
    {10.561 -1.070/2  ,            1.070,         -3.895,0.750},  // r for center
    {10.561 -1.070/2  ,            1.070,          3.895,0.750},  // r for center
    {1.050+2.515+21.915,2*(25.950-(1.050+2.515+21.915)), 0., 2}   // r for center
  };
  RIB_t RIBO[14] = { // Drawing 24A3925G
    { 0.280,     1.5480 - 0.280, 0., 0.}, // r for upper edge
    { 4.261,              0.375, 0., 0.},
    { 7.410,              0.375, 0., 0.},
    {10.560,              0.375, 0., 0.},
    {13.710,              0.375, 0., 0.},
    {16.859,              0.375, 0., 0.},
    {20.009,              0.375, 0., 0.},
    {23.158,              0.375, 0., 0.},
    {26.309, 27.595+0.28-26.309, 0., 0.},
    {( 7.410+ 4.261+0.375)/2,( 7.410- 4.261-0.375), 17.250/2, 1.250}, // r for center
    {( 7.410+ 4.261+0.375)/2,( 7.410- 4.261-0.375),-17.250/2, 1.250},
    {(13.710+10.560+0.375)/2,(13.710-10.560-0.375),  0.0    , 0.375},
    {(16.859+13.710+0.375)/2,(16.859-13.710-0.375),  0.0    , 0.375},
    {26.309-1.575/2         , 1.575               ,  0.0, (26.309-6.565-18.960)*2}
  };
  RIB_t *RIB[2] = {RIBI, RIBO};
  Int_t noHolesRows[2] = {15, 16};
  Int_t noHolesPerRowI[15] = {6, 5, 5, 2, 5,   // 0 - 4	 
			      4, 4, 4, 4, 3,   // 5 -9 	 
			      3, 3, 3, 2, 2};  // 10 - 14
  Int_t noHolesPerRowO[16] = {9, 9, 9, 9, 9,   // 0 - 4	 
			      8, 8, 8, 8, 8,   // 5 -9 	 
			      7, 7, 7, 7, 7,   // 10 - 14
			      6};              // 15     
  Int_t *noHolesPerRow[2] = {noHolesPerRowI, noHolesPerRowO};

  Double_t yhI[15*9];
  memset (yhI, 0, 15*9*sizeof(Double_t));
  i = 0; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.180*(-2.5 + j);}
  i = 1; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.764*(-2.0 + j);}
  i = 2; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.553*(-2.0 + j);}
  i = 3; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] = 13.368*(-0.5 + j);}
  i = 4; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.131*(-2.0 + j);}
  i = 5; 
  yhI[9*i+0] = - 3.892 - 3.896/2;  yhI[9*i+3] = - yhI[9*i+0];
  yhI[9*i+1] =         - 3.896/2;  yhI[9*i+2] = - yhI[9*i+1];
  i = 6; 
  yhI[9*i+0] = - 3.611 - 3.614/2;  yhI[9*i+3] = - yhI[9*i+0];
  yhI[9*i+1] =         - 3.614/2;  yhI[9*i+2] = - yhI[9*i+1];
  i = 7; 
  yhI[9*i+0] = - 3.281 - 3.429/2;  yhI[9*i+3] = - yhI[9*i+0];
  yhI[9*i+1] =         - 3.429/2;  yhI[9*i+2] = - yhI[9*i+1];
  i = 8; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.150*(-1.5 + j);}
  i = 9; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  4.152*(-1.0 + j);}
  i =10; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.730*(-1.0 + j);}
  i =11; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.308*(-1.0 + j);}
  i =12; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  3.075*(-1.0 + j);}
  i =13; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  4.927*(-0.5 + j);}
  i =14; for (j = 0; j < noHolesPerRowI[i]; j++) {yhI[9*i+j] =  4.083*(-0.5 + j);}
  Double_t xHoles[16];
  Double_t yhO[16*9] = { // [16][9] inches 
    -15.025, -11.606, -8.177, -4.220,  0,  4.220,  8.177,  11.606, 15.025,    // 0
    -15.025, -11.606, -8.177, -4.220,  0,  4.220,  8.177,  11.606, 15.025,    // 1
    -14.243, -11.078, -6.330, -3.165,  0,  3.165,  6.330,  11.078, 14.243,    // 2
    -14.243, -11.078, -6.330, -3.165,  0,  3.165,  6.330,  11.078, 14.243,    // 3
    -13.452, -10.023, -6.330, -3.165,  0,  3.165,  6.330,  10.023, 13.452,    // 4
    -13.452, -10.023, -6.330, -3.165,      3.165,  6.330,  10.023, 13.452, 0, // 5
    -12.660,  -9.495, -6.330, -2.110,      2.110,  6.330,   9.495, 12.660, 0, // 6
    -12.660,  -9.495, -6.330, -2.110,      2.110,  6.330,   9.495, 12.660, 0, // 7
    -11.870,  -8.440, -5.275, -2.110,      2.110,  5.275,   8.440, 11.870, 0, // 8
    -11.870,  -8.440, -5.275, -2.110,      2.110,  5.275,   8.440, 11.870, 0, // 9
    -11.078,  -7.649, -4.220,          0,  4.220,  7.649,  11.078,      0, 0, // 10
    -11.078,  -7.649, -4.220,          0,  4.220,  7.649,  11.078,      0, 0, // 11
    -10.022,  -6.858, -3.429,          0,  3.429,  6.858,  10.022,      0, 0, // 12
    -10.022,  -6.858, -3.429,          0,  3.429,  6.858,  10.022,      0, 0, // 13
    -9.495,   -6.330, -3.165,          0,  3.165,  6.330,   9.495,      0, 0, // 14
    -9.495,   -6.330, -3.165,              3.165,  6.330,   9.495,  0,  0, 0};// 15
  Double_t *yHoles[2] = {yhI, yhO};
  //  TpcSector->SetVisibility(1);
  TGeoVolumeMulti *TpcSectorAlSupport = gGeoManager->MakeVolumeMulti("TpcSectorAlSupport", GetMed("TPCE_ALUMINIUM"));
  TpcSectorAlSupport->SetLineColor(kBlue);
  TpcSectorAlSupport->SetTitle("the Sector G10 alliminium support");
  TGeoVolumeMulti *TpcSectorG10 = gGeoManager->MakeVolumeMulti("TpcSectorG10", GetMed("TPCE_G10"));
  TpcSectorG10->SetLineColor(kYellow);
  TpcSectorG10->SetTitle("the Sector G10");
  TGeoVolumeMulti *TRIB = gGeoManager->MakeVolumeMulti("TRIB", GetMed("TPCE_ALUMINIUM"));
  TRIB->SetTitle("the Sector alliminium support ribs");
  TRIB->SetLineColor(kBlue);
  TGeoVolumeMulti *WireMount = gGeoManager->MakeVolumeMulti("WireMount", GetMed("TPCE_G10"));
  WireMount->SetTitle("the Sector Wire support");
  WireMount->SetLineColor(kYellow);
  TGeoVolumeMulti *THOLE = gGeoManager->MakeVolumeMulti("THOLE", GetMed("TPCE_STANDARD"));
  THOLE->SetTitle("the Sector holes for FEE");
  /* A.Lebedev 10/31/2008 
                                          Hole:   66x9.5x19
     Socket soldered to sector to connect FEE:    57x5.8x6.2(height)mm**3 
     Brackets on both sides of outer sector to hold wires(gating grid, cathode, anodes)    1.93kG 
     DAQ1000 RDO size 45*17 cm, G10 + chips
     DAQ1000 FEEs for supersector (inner+ outer sector) 26kG,
     DAQ1000 FEE cables for supersector 9.0 lb
     FEE cooling maniford: 8.0 lb - inner sector, 15 lb - outer sector
     Outer sector weight = 47 kG
  */
  TGeoVolume *TFEESocket = gGeoManager->MakeBox("TFEESocket", GetMed("TPCE_G10"), 6.2/20, 57./20, 5.8/20);
  TFEESocket->SetTitle("Socket soldered to sector to connect FEE");
  // FEE Assembly Drawing 24A6552A
  Double_t FeeCardDX  = 1.47*0.110*inch/2; // 1.47 scale factor account for 9 lb of cables
  Double_t FeeCardDY  = 2.900*inch/2;
  Double_t FeeCardDZ  = 7.000*inch/2;
  Double_t FeePlateDX = 0.110*inch/2;
  Double_t FeePlateDY = 1.480*inch;
  Double_t FeePlateDZ = 4.650*inch/2;
  Double_t FEEAssemblyThickness = FeeCardDX + FeePlateDX;
  Double_t FEERibDX   = 0.820*inch/2 - 2*FeeCardDX;
  Double_t FEERibDY   = 2.900*inch/2 - 2*FeeCardDX;
  Double_t FEERibDZ   = FeeCardDX;
  TGeoVolume *FEEplate = gGeoManager->MakeBox("FEEPlate",  GetMed("TPCE_ALUMINIUM"), FeePlateDX, FeePlateDY, FeePlateDZ);
  FEEplate->SetLineColor(kBlue);
  TGeoVolume *FEERib   = gGeoManager->MakeBox("FEERib",    GetMed("TPCE_ALUMINIUM"), FEERibDX,  FEERibDY,  FEERibDZ);
  FEERib->SetLineColor(kBlue);
  TGeoVolume *FEEitself= gGeoManager->MakeBox("FEEitself", GetMed("TPCE_G10"),       FeeCardDX, FeeCardDY, FeeCardDZ);
  FEEitself->SetLineColor(kYellow);
  TGeoVolumeAssembly *FEE = new TGeoVolumeAssembly("FEE"); // Weight = 181*FEE = 26 kG (A.Lebedev)
  FEE->AddNode(FEEplate,  1, gGeoIdentity);                // Cables = 9 lb ( - " -)
  FEE->AddNode(FEERib,    1, new TGeoTranslation(-FEERibDX-FeeCardDX,0,0));
  FEE->AddNode(FEEitself, 1, new TGeoTranslation(2*FeeCardDX,0,0.5));
  TGeoVolumeMulti *CoolingTube = gGeoManager->MakeVolumeMulti("CoolingTube", GetMed("TPCE_Water_Pipe")); 
  CoolingTube->SetLineColor(kViolet);
  // Cooling TUBE Drawing 24A0801B  Mixture TPCE_Water_Pipe => rho = 2.32155 g/cm**3
  Double_t heigTube = 0.703*inch;  // x 
  Double_t  widTube = 0.500*inch;  // z
  // Inner/Outer         {   24A3696A,     24A4296A} Sector Cooling Manifold Overall Dimensions
  Double_t width1IO[2] = { 7.661*inch, 22.287*inch};
  Double_t width2IO[2] = {20.552*inch, 35.422*inch};
  Double_t heightIO[2] = {24.222*inch, 24.840*inch};
  Double_t depthIO[2]  = { 1.800*inch,  1.800*inch};
  // FEE + Cooling manifold Assembly Drawings 24A0882A
  // Cooling Drawing 24A4096C
  Double_t InnerCoolingTubeLength[15] = {// 15 degrees upper edge
    19.300, 18.454, 17.610, 16.766, 15.922,
    15.078, 14.235, 13.390, 12.546, 11.704,
    10.860, 10.016,  9.172,  8.327,  7.483};
  
  Double_t OuterCoolingTubeLength[16] = {// 15 degrees upper edge
    33.620, 33.605, 32.761, 31.917,
    31.073, 30.229, 29.385, 28.541,
    27.697, 26.853, 26.009, 25.165,
    24.321, 23.478, 22.634, 21.790};
  Double_t *CoolingTubeLength[2] = {InnerCoolingTubeLength, OuterCoolingTubeLength};
  // FEE cooling
  
  TGeoVolumeMulti *TPAD = gGeoManager->MakeVolumeMulti("tpad", GetMed("TPCE_SENSITIVE_GAS")); 
  TPAD->SetTitle("a real padrow with dimensions defined at positioning time");
  TPAD->SetVisibility(1);
  TPAD->SetLineColor(kPink);
  TGeoVolumeMulti *GatingGrid = gGeoManager->MakeVolumeMulti("GatingGrid", GetMed("TPCE_P10")); 
  GatingGrid->SetTitle("dead region between Gating Grid and Cathode wires");
  GatingGrid->SetVisibility(1);
  GatingGrid->SetLineColor(kRed);
  TGeoVolumeAssembly *tpcPadPlane[2];
  TGeoVolumeAssembly *tpcSector[2];
  TGeoVolumeAssembly *tpcWheel[2];
  TGeoVolume *pad = 0;
  for (sec = 0; sec < 2; sec++) {
    tpcPadPlane[sec] = new TGeoVolumeAssembly("TpcPadPlane");
    tpcPadPlane[sec]->SetTitle(Form("Tpc%sPadPlane",InnerOuter[sec]));
    z2 = zWheel1;
    z1 = z2 - TECW[sec].Thick;
    z  = (z1 + TPCG.MembTHK/2)/2;
    dz = (z1 - TPCG.MembTHK/2)/2;
    Double_t distanceGG2AnodeWire = 0.6;
    Double_t zGG = dz - 2*TPRS[sec].dAnode-distanceGG2AnodeWire;
    Double_t zOfDeadRegion = 195 - z;  // dead region z = [195, zGG]
    cout << "zGG " << zGG << "\tzOfDeadRegion " << zOfDeadRegion << endl;
    dx = TPRS[sec].width/2;
    for (Int_t row = 1; row <= TPRS[sec].nRow; row++) {
      Int_t Id = (! sec ) ? row : TPRS[sec-1].nRow + row;
      TGeoVolume *gatingGrid = gGeoManager->MakeBox("GatingGrid",GetMed("TPCE_P10"),
						    TPRS[sec].Npads[row-1]*TPRS[sec].pitch/2,TPRS[sec].width/2, 
						    (zGG - zOfDeadRegion)/2);
      GatingGrid->AddVolume(gatingGrid);
      dy = TPRS[sec].Npads[row-1]*TPRS[sec].pitch/2;
      pad = gGeoManager->MakeBox("tpad",GetMed("TPCE_SENSITIVE_GAS"),dy,dx,dz);
      TPAD->AddVolume(pad);
      pad->AddNode(gatingGrid, 1, new TGeoTranslation(0,0,(zGG + zOfDeadRegion)/2));
      if (sec == 0 || row == 1) {
	x = TPRS[sec].Rpads[row-1] - TPRS[sec].width;
	tpcPadPlane[sec]->AddNode(pad,100000 + Id,new TGeoCombiTrans(x,0,z,GetRot("Z180")));
      }
      x = TPRS[sec].Rpads[row-1];
      tpcPadPlane[sec]->AddNode(pad,Id,new TGeoCombiTrans(x,0,z,GetRot("Z180")));
      if (sec == 0 || row == TPRS[sec].nRow) {
	x = TPRS[sec].Rpads[row-1] + TPRS[sec].width;
	tpcPadPlane[sec]->AddNode(pad,200000 + Id,new TGeoCombiTrans(x,0,z,GetRot("Z180")));
      }
    }
    // Outer and Inned Pad Plane dimensions

    // Sectors
    x1 = -TECW[sec].Height/2;
    x2 =  TECW[sec].Height/2;
    y1 =  TECW[sec].inwidth/2;
    y2 =  TECW[sec].ouwidth/2;
    x0 =  (y1*x2-y2*x1)/(x2-x1);
    k  =  (y2 - y1)/(x2 - x1);
    alpha = 180./TMath::Pi()*TMath::ATan(k);
    r1 = y1/k;
    r2 = y2/k;
    dRSectorShift = TECW[sec].rMin - r1;
    SectorItSelves[sec] = new TGeoPgon(-alpha,2*alpha,1,2);
    SectorItSelves[sec]->SetName(Form("%sSectorItSelf",InnerOuter[sec]));
    z2 = zWheel1;
    z1 = z2 - TECW[sec].Thick;
    SectorItSelves[sec]->DefineSection(0, z1, r1, r2); 
    SectorItSelves[sec]->DefineSection(1, z2, r1, r2);             
    SectorShifts[sec] = new TGeoTranslation(dRSectorShift,0,0);
    SectorShifts[sec]->SetName(Form("%sShift",InnerOuter[sec])); SectorShifts[sec]->RegisterYourself();// SectorShifts[sec]->Print();
    x = 0;
/*     cout << "  x1 = " << x1 << "\tx2 = " << x2  */
/* 	 << "\ty1 = " << y1 << "\ty2 = " << y2  */
/* 	 << "\tx0 = " << x0 << "\tk = " << k << "\talpha = " << alpha << endl; */
    tpcSector[sec] = new TGeoVolumeAssembly(Form("Tpc%sSectorAssembly",InnerOuter[sec]));
    tpcSector[sec]->SetTitle(Form("the %s sector",InnerOuter[sec]));
    tpcWheel[sec] = new TGeoVolumeAssembly(Form("TpcWheel%sAssembly",InnerOuter[sec]));
    tpcWheel[sec]->SetTitle(Form("the %s wheel assembly",InnerOuter[sec]));
    TGeoVolume *tpcSectorAlSupport = gGeoManager->MakePgon("TpcSectorAlSupport",GetMed("TPCE_ALUMINIUM"),-alpha,2*alpha,1,2);
    tpcSectorAlSupport->SetTitle("the Sector G10 alliminium support");
    tpcSector[sec]->SetLineColor(kRed);
    //    tpcSector[sec]->SetVisibility(1);
    pgon = (TGeoPgon *) tpcSectorAlSupport->GetShape();
    pgon->DefineSection(0, z1+PadPlaneThickness                  , r1, r2); 
    pgon->DefineSection(1, z1+PadPlaneThickness+TECW[sec].ThickAl, r1, r2);             
    TpcSectorAlSupport->AddVolume(tpcSectorAlSupport);
    tpcSector[sec]->AddNode(tpcSectorAlSupport,1,gGeoIdentity);
    TGeoVolume *tpcSectorG10 = gGeoManager->MakePgon("TpcSectorG10",GetMed("TPCE_G10"),-alpha,2*alpha,1,2);
    tpcSectorG10->SetTitle("the Sector G10");
    tpcSectorG10->SetLineColor(kYellow);
    pgon = (TGeoPgon *) tpcSectorG10->GetShape();
    pgon->DefineSection(0, z1                  , r1, r2); 
    pgon->DefineSection(1, z1+PadPlaneThickness, r1, r2);             
    TpcSectorG10->AddVolume(tpcSectorG10);
    tpcSector[sec]->AddNode(tpcSectorG10,1,gGeoIdentity);
    // Holes ignore radii
    if (sec == 0) { 
      dx = 0.750/2*inch; // = 0.9525 cm
      dy = 2.625/2*inch; // = 3.33375 cm
      xHoles[0] = r2 - 1.988*inch;
      for (i = 1; i < noHolesRows[sec]; i++) xHoles[i] = xHoles[i-1] - 1.575*inch;
    } else {
      dx = 0.750/2*inch;
      dy = 2.500/2*inch;
      for (i = 0; i < noHolesRows[sec]; i++) {
	xHoles[i] = r2 - (1.988 + 1.575*i)*inch;
      }
    }
    dz = TECW[sec].ThickAl/2;
    TGeoVolume *thole = gGeoManager->MakeBox("THOLE", GetMed("TPCE_STANDARD"), dx, dy, dz);
    THOLE->AddVolume(thole);
    thole->AddNode(TFEESocket, 1, gGeoIdentity);
    const Char_t *LeftRight[2] = {"Left","Right"};
    // Ribs
    dy = (r2 - r1)/2;
    y  = (r2 + r1)/2;
    dx = TECW[sec].widthRib/2;
    dz = (TECW[sec].Thick - PadPlaneThickness - TECW[sec].ThickAl)/2;
    z  = z2 - dz;
    Double_t dxW = WireMountWidth/2;
    Double_t dzW = WireMountHeight/2;
    Double_t xW = 0;
    for (i = 0; i < 2; i++) {
      TGeoVolume *trib = gGeoManager->MakePara("TRIB", GetMed("TPCE_ALUMINIUM"), dx, dy, dz,-(1-2*i)*alpha, 0, 0);
      trib->SetLineColor(kBlue);
      //      trib->SetVisibility(1);
      TRIB->AddVolume(trib);
      x  = ((y1 + y2)/2 - dx)*(1-2*i);
      tpcSector[sec]->AddNode(trib,i+1, new TGeoCombiTrans(y,-x, z,GetRot("YZXZ")));
      //      trib->InspectShape();trib->InspectMaterial(); printf("%s\n",trib->GetTitle());
      xW = ((y1 + y2)/2 +  dxW)*(1-2*i);
      TGeoVolume *wireMount = gGeoManager->MakePara("WireMount", GetMed("TPCE_G10"), dxW, dy, dzW,-(1-2*i)*alpha, 0, 0);
      wireMount->SetLineColor(kYellow); 
      //      wireMount->SetVisibility(1);
      WireMount->AddVolume(wireMount);
      para = (TGeoPara *) wireMount->GetShape();
      para->SetName(Form("WireMount%s%s",InnerOuter[sec],LeftRight[i]));
      Double_t zW = z1 - dzW/2;
      TGeoCombiTrans *rt = new TGeoCombiTrans(y,-xW, zW,GetRot("YZXZ"));
      rt->SetName(Form("WireMountTR%s%s",InnerOuter[sec],LeftRight[i]));
      rt->RegisterYourself();
      tpcSector[sec]->AddNode(wireMount,i+1, rt);
      //      wireMount->InspectShape();wireMount->InspectMaterial(); printf("%s\n",wireMount->GetTitle());
    }
    const Char_t *io = InnerOuter[sec];
    new TGeoCompositeShape(Form("%sSector",io),
			   Form("%sSectorItSelf + WireMount%sLeft:WireMountTR%sLeft + WireMount%sRight:WireMountTR%sRight",
				io,io,io,io,io));
    //
    r = r2;
    for (i = 0; i < TECW[sec].noribs; i++) {
      r  = r2 - RIB[sec][i].x*inch;
      dr = RIB[sec][i].dx*inch;
      y  = RIB[sec][i].y*inch;
      dy = RIB[sec][i].dy*inch;
      
      //      cout << "rib i = " << i << "\tr = " << r << "\tdr = " << dr << "\ty = " << y << "\tdy = " << dy << endl;
      if (dy < 1.e-7) {
	dx2 =  r      *k - TECW[sec].widthRib - 2*dxW; //TECW[sec].widthLip;
	dx1 = (r - dr)*k - TECW[sec].widthRib - 2*dxW; //TECW[sec].widthLip;
	xc = r - TECW[sec].zStepRib/2;
/* 	cout << "r = " << r << "\txc = " << xc << "\tdr = " << dr << "\tXc = " << xc - r1  */
/* 	     << "\tdx1 = " << dx1 << "\tdx2 = " << dx2 << "\tdz = " << dz << endl; */
	TGeoVolume *trib = gGeoManager->MakeTrd1("TRIB", GetMed("TPCE_ALUMINIUM"), dx1, dx2, dz, dr/2);
	trib->SetLineColor(kBlue);
	//      trib->SetVisibility(1);
	tpcSector[sec]->AddNode(trib,i+3, new TGeoCombiTrans(r-dr/2, 0, z2-dz, GetRot("90XD")));
	//	trib->InspectShape();trib->InspectMaterial(); printf("%s\n",trib->GetTitle());
      } else {
	TGeoVolume *trib = gGeoManager->MakeBox("TRIB", GetMed("TPCE_ALUMINIUM"), dr/2, dy/2, dz);
	trib->SetLineColor(kBlue);
	TRIB->AddVolume(trib);
	tpcSector[sec]->AddNode(trib,i+3,new TGeoTranslation(r,y, z2-dz));
	//	trib->InspectShape();trib->InspectMaterial(); printf("%s\n",trib->GetTitle());
      }
    }
    // put holes, cooling tube and FEE
    //    cout << "put holes in " << noHolesRows[sec] << " rows for sector\t" << sec << endl; 
    Double_t X = 0;
    Double_t Y = 0;
    TGeoVolume *coolingTube = 0;
    dx =   widTube*cm/2;
    dz =   depthIO[sec]/2;
    dy =   heightIO[sec]/2;
    y1 = - dy;
    y2 =   dy;
    x1 =  width1IO[sec]/2 - dx;
    x2 =  width2IO[sec]/2 - dx;
    x0 =  (y1*x2-y2*x1)/(y2-y1);
    k  =  (x2 - x1)/(y2 - y1);
    Double_t beta = 180./TMath::Pi()*TMath::ATan(k);
    //    cout << "x0 " << x0 << " k " << k << " beta " << beta << endl;
    if (sec == 1) dy -= 0.7;
    Y  = xHoles[0] - dy;
    X = (x1 + x2)/2;
    for (i = 0; i < 2; i++) {
      coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), dx, dy, dz,-(1-2*i)*beta, 0, 0);
      coolingTube->SetLineColor(kViolet);
      CoolingTube->AddVolume(coolingTube);                           //  "YZXZ" :  (x,y,z) => ( y, x,-z)  
      tpcWheel[sec]->AddNode(coolingTube, 2*sec+i+1, new TGeoCombiTrans(Y,X*(2*i-1),zWheel1+ dz,GetRot("YZXZ")));
    }
    for (i = 0; i < noHolesRows[sec]; i++) {
      X = xHoles[i]; // cm
      // ?      if (sec == 1 && i == 0) X -= 0.3;
      //      cout << "sec = " << sec << "\thole " << i << "\tX = " << X << endl;
      dx = heigTube*cm/2;
      dz = widTube*cm/2;
      dy = CoolingTubeLength[sec][i]*inch/2;
      Double_t dy2 = dy + 2*k*dx;
      Double_t dy1 = dy;
      if (sec == 1) {
	dy2 = dy;
	dy1 = dy - 2*k*dx;
      }
      //      cout << " dx " << dx << " dy " << dy << " dy1 " << dy1 << " dy2 " << dy2 << " dz " << dz << endl; 
      coolingTube = gGeoManager->MakeTrd1("CoolingTube", GetMed("TPCE_Water_Pipe"), dy1, dy2, dz, dx);
      coolingTube->SetLineColor(kViolet);
      CoolingTube->AddVolume(coolingTube);
      tpcWheel[sec]->AddNode(coolingTube, 2*i+sec+5, // "90XD" : (x,y,z) => ( y, z, x)
			     new TGeoCombiTrans(X-dx-FEEAssemblyThickness/2+dRSectorShift,0, zWheel1+ dz,GetRot("90XD")));
      for (j = 0; j <  noHolesPerRow[sec][i]; j++) {
	Y = yHoles[sec][9*i+j]*inch;
	//	cout << "Y = " << Y << endl;
	tpcSectorAlSupport->AddNode(thole,9*i+j+1, new TGeoTranslation(X,Y,z1+PadPlaneThickness+TECW[sec].ThickAl/2));	    
	tpcWheel[sec]->AddNode(FEE,2*(9*i+j)+sec+1, 
			       new TGeoTranslation(X+FEEAssemblyThickness/2+dRSectorShift,Y,zWheel1+ 2*dz+FEERibDZ));	    
      }
    }
    // Wheel Gap is Air gap in the TPC supporting endcap Wheel
    x1 = -TECW[sec].GapHeit/2;
    x2 =  TECW[sec].GapHeit/2;
    y1 =  TECW[sec].GapWidI/2;
    y2 =  TECW[sec].GapWidO/2;
    x0 =  (y1*x2-y2*x1)/(x2-x1);
    k  =  (y2 - y1)/(x2 - x1);
    alpha = 180./TMath::Pi()*TMath::ATan(k);
    r1 = y1/k;
    r2 = y2/k;
    dRWheelShift = TECW[sec].GapRad - (r1+r2)/2;
    WheelShifts[sec] = new TGeoTranslation(dRWheelShift,0,0);
    WheelShifts[sec]->SetName(Form("%sGapShift",InnerOuter[sec]));  WheelShifts[sec]->RegisterYourself();
    // WheelShifts[sec]->Print();
 }
  new TGeoCompositeShape("Sectors","InnerSector:InnerShift + OuterSector:OuterShift");
  TGeoPcon *GasAndSector = new TGeoPcon(-15,30,2);
  GasAndSector->SetName("GasAndSector");
  GasAndSector->DefineSection(0,  TPCG.MembTHK/2, tifcOR, tofcIR);        // membrane 
  GasAndSector->DefineSection(1,         zWheel1, tifcOR, tofcIR);        // gas volume + sector
  TGeoCompositeShape *Gas = new TGeoCompositeShape("Gas","GasAndSector - Sectors"); 
  TGeoVolume *TpcGas = new TGeoVolume("TpcGas",Gas,GetMed("TPCE_P10"));
  TpcGas->SetTitle("the P10 gas volume placed in a (whole) sector"); 
  TpcGas->SetVisibility(0);
  TpcGas->SetLineColor(kBlack);
  TpcSectorWhole->AddNode(TpcGas,1,gGeoIdentity);
  // Fill TpcGas Volume
  for (sec = 0; sec <2; sec++) {
    TpcGas->AddNode(tpcPadPlane[sec],1+sec,gGeoIdentity);
  }
  pcon = new TGeoPcon(-15,30,2);
  pcon->SetName("TpcSectorApronSheetCyl");
  pcon->DefineSection(0,TPCG.LengthV/2-0.7, tofcIR-8.45, tofcIR-1.77); 
  pcon->DefineSection(1,TPCG.LengthV/2-0.4, tofcIR-8.45, tofcIR-1.77); 
  pgon = new TGeoPgon(-15,30,1,2);
  pgon->SetName("TpcSectorApronSheetFlat");
  pgon->DefineSection(0,TPCG.LengthV/2-0.7, (tofcIR-8.45)*TMath::Cos(15./180.*TMath::Pi()), tofcIR-8.45); 
  pgon->DefineSection(1,TPCG.LengthV/2-0.4, (tofcIR-8.45)*TMath::Cos(15./180.*TMath::Pi()), tofcIR-8.45); 
  TGeoCompositeShape *TpcSectorApronSheet = new TGeoCompositeShape("TpcSectorApronSheet","TpcSectorApronSheetCyl - TpcSectorApronSheetFlat");
  TGeoVolume *TSAS = new TGeoVolume("TpcSectorApronSheet",TpcSectorApronSheet,GetMed("TPCE_ALUMINIUM"));
  TSAS->SetTitle("the Tpc sector apron flat part"); 
  TSAS->SetVisibility(1);
  TSAS->SetLineColor(kBlue);
  TpcGas->AddNodeOverlap(TSAS,1,gGeoIdentity);

  TGeoVolume *TSAC = gGeoManager->MakePgon("TpcSectorApronCylinder",GetMed("TPCE_ALUMINIUM"),-15,30,1,2);
  pgon = (TGeoPgon *) TSAC ->GetShape();
  pgon->DefineSection(0,TPCG.LengthV/2-0.4, tofcIR-8.85,tofcIR-8.45); 
  pgon->DefineSection(1,           zWheel1, tofcIR-8.85,tofcIR-8.45); 
  TSAC->SetTitle("the Tpc sector apron cylinder part"); 
  TSAC->SetVisibility(1);
  TSAC->SetLineColor(kBlue);
  TpcGas->AddNodeOverlap(TSAC,1,gGeoIdentity);

  // Wheel itself
  TGeoPcon *WheelRegion = new TGeoPcon(-15,30,6);
  WheelRegion->DefineSection(0,         zWheel1, tifcOR, TPCG.WheelOR1); // wheel begins
  WheelRegion->DefineSection(1,        zTOFCend, tifcOR, TPCG.WheelOR1); 
  WheelRegion->DefineSection(2,        zTOFCend, tifcOR, TPCG.WheelOR); // wheel with TOFC 
  WheelRegion->DefineSection(3,zTIFCFlangeBegin, tifcOR, TPCG.WheelOR); // wheel with TIFC flange
  WheelRegion->DefineSection(4,zTIFCFlangeBegin, TPCG.tifcRF+TPCG.tifcDRT, TPCG.WheelOR);
  WheelRegion->DefineSection(5,  TPCG.LengthT/2, TPCG.tifcRF+TPCG.tifcDRT, TPCG.WheelOR);
  WheelRegion->SetName("WheelRegion");
  TGeoCompositeShape *SectorsAndWheel = new TGeoCompositeShape("SectorsAndWheel","Sectors + WheelRegion");
  TGeoVolume *TpcSectorAndWheel = new TGeoVolume("TpcSectorAndWheel",SectorsAndWheel,GetMed("TPCE_STANDARD"));
  TpcSectorAndWheel->SetTitle("the TPC Sector and supporting endcap Wheel");
  TpcSectorAndWheel->SetLineColor(kRed);
  TpcSectorAndWheel->SetVisibility(1);
  TpcSectorWhole->AddNode(TpcSectorAndWheel,1,gGeoIdentity);
  for (sec = 0; sec < 2; sec++) {
    TpcSectorAndWheel->AddNode(tpcSector[sec],sec+1, SectorShifts[sec]);
    TpcSectorAndWheel->AddNode(tpcWheel[sec],sec+1, gGeoIdentity);
  }
  TGeoVolume *WheelBottom = gGeoManager->MakePcon("WheelBottom",GetMed("TPCE_ALUMINIUM"),-15,30,4);
  pcon = (TGeoPcon *) WheelBottom ->GetShape();
  pcon->DefineSection(0,           zWheel1,     TPCG.WheelIR, TPCG.WheelR0); // wheel begins
  pcon->DefineSection(1,zWheel1+3.500*inch,     TPCG.WheelIR, TPCG.WheelR0); // wheel begins
  pcon->DefineSection(2,zWheel1+3.500*inch, TPCG.WheelIR+3.6, TPCG.WheelR0); // wheel begins
  pcon->DefineSection(3,    TPCG.LengthW/2, TPCG.WheelIR+3.6, TPCG.WheelR0); // wheel begins
  TpcSectorAndWheel->AddNode(WheelBottom,1,gGeoIdentity);
  WheelBottom->SetLineColor(kBlue);
  TGeoPcon *WheelOuterPcon = new TGeoPcon(-15,30,4);
  WheelOuterPcon->SetName("WheelOuterPcon");
  WheelOuterPcon->DefineSection(0,         zWheel1, TPCG.WheelR2, TPCG.WheelOR1); // wheel begins
  WheelOuterPcon->DefineSection(1,        zTOFCend, TPCG.WheelR2, TPCG.WheelOR1); 
  WheelOuterPcon->DefineSection(2,        zTOFCend, TPCG.WheelR2, TPCG.WheelOR); // wheel with TOFC 
  WheelOuterPcon->DefineSection(3,  TPCG.LengthW/2, TPCG.WheelR2, TPCG.WheelOR); // wheel ends
  TGeoPgon *WheelOuterPgon = new TGeoPgon(-15,30,1,2);
  WheelOuterPgon->SetName("WheelOuterPgon");
  WheelOuterPgon->DefineSection(0,         zWheel1, TPCG.WheelR2*cos15, TPCG.WheelR2); // wheel begins
  WheelOuterPgon->DefineSection(1,  TPCG.LengthW/2, TPCG.WheelR2*cos15, TPCG.WheelR2); 
  TGeoCompositeShape *wheelOuterRing = new TGeoCompositeShape("WheelOuterRing","WheelOuterPcon - WheelOuterPgon");
  TGeoVolume *WheelOuterRing = new TGeoVolume("WheelOuterRing",wheelOuterRing,GetMed("TPCE_ALUMINIUM"));
  TpcSectorAndWheel->AddNode(WheelOuterRing,1,gGeoIdentity);
  // Ribs
  TGeoVolumeMulti *WheelRib = gGeoManager->MakeVolumeMulti("WheelRib",GetMed("TPCE_ALUMINIUM"));
  WheelRib->SetVisibility(1);
  WheelRib->SetLineColor(kBlue);
  TGeoVolume *wheelRib = 0;
  wheelRib = gGeoManager->MakePgon("TpcWheelTopRib",GetMed("TPCE_ALUMINIUM"), -15, 30,1,2);
  wheelRib->SetTitle("TpcWheelTopRib");
  wheelRib->SetLineColor(kBlue);
  pgon = (TGeoPgon *) wheelRib->GetShape();
  pgon->DefineSection(0,zWheel1                    , TPCG.WheelR2 - (TPCG.WheelTotalRibWidth-TPCG.WheelRibWidth)/2, TPCG.WheelR2); 
  pgon->DefineSection(1,zWheel1+TPCG.WheelRibHeight, TPCG.WheelR2 - (TPCG.WheelTotalRibWidth-TPCG.WheelRibWidth)/2, TPCG.WheelR2); 
  WheelRib->AddVolume(wheelRib);
  TpcSectorAndWheel->AddNode(wheelRib,1,gGeoIdentity);

  wheelRib = gGeoManager->MakePgon("TpcWheelMiddleRib",GetMed("TPCE_ALUMINIUM"), -15, 30,1,4);
  wheelRib->SetTitle("TpcWheelMiddleRib");
  wheelRib->SetLineColor(kBlue);
  pgon = (TGeoPgon *) wheelRib->GetShape();
  pgon->DefineSection(0,zWheel1                    , TPCG.WheelR1 - TPCG.WheelTotalRibWidth/2, TPCG.WheelR1 + TPCG.WheelTotalRibWidth/2); 
  pgon->DefineSection(1,zWheel1+TPCG.WheelRibHeight, TPCG.WheelR1 - TPCG.WheelTotalRibWidth/2, TPCG.WheelR1 + TPCG.WheelTotalRibWidth/2); 
  pgon->DefineSection(2,zWheel1+TPCG.WheelRibHeight, TPCG.WheelR1 - TPCG.WheelRibWidth/2     , TPCG.WheelR1 + TPCG.WheelRibWidth/2); 
  pgon->DefineSection(3,TPCG.LengthW/2             , TPCG.WheelR1 - TPCG.WheelRibWidth/2     , TPCG.WheelR1 + TPCG.WheelRibWidth/2); 
  WheelRib->AddVolume(wheelRib);
  TpcSectorAndWheel->AddNode(wheelRib,2,gGeoIdentity);
  TGeoVolume *WheelRibBox = gGeoManager->MakeBox("WheelRibBox",GetMed("TPCE_ALUMINIUM"), TPCG.WheelBoxDy/2, TPCG.WheelBoxDx/2, TPCG.WheelTHK/2);
  WheelRibBox->SetLineColor(kBlue);
  TpcSectorAndWheel->AddNodeOverlap(WheelRibBox, 1, new TGeoTranslation(TPCG.WheelR1, 0., zWheel1+TPCG.WheelTHK/2)); 
  for (i = 0; i < 2; i++) {
    x1 = TPCG.WheelR0*cos15;
    x2 = TPCG.WheelR2;
    dz = TPCG.WheelRibHeight/2;
    z  = zWheel1 + dz;
    dx = (x2 - x1)/2;
    dy = TPCG.WheelTotalRibWidth/4;
    x  = (x1 + x2)/2;
    y  = (x *tan15 - dy)*(2*i-1);
    wheelRib = gGeoManager->MakePara("TpcWheelRib", GetMed("TPCE_ALUMINIUM"), dy, dx, dz,-(1-2*i)*15, 0, 0);
    wheelRib->SetLineColor(kBlue);
    //      wheelrib->SetVisibility(1);
    WheelRib->AddVolume(wheelRib);
    TpcSectorAndWheel->AddNodeOverlap(wheelRib,3+i, new TGeoCombiTrans(x, y, z,GetRot("YZXZ"))); 
    dz = TPCG.WheelTHK/2;
    z  = zWheel1 + dz;
    dy = TPCG.WheelRibWidth/4;
    x  = (x1 + x2)/2;
    y  = (x *tan15 - dy)*(2*i-1);
    wheelRib = gGeoManager->MakePara("TpcWheelRib", GetMed("TPCE_ALUMINIUM"), dy, dx, dz,-(1-2*i)*15, 0, 0);
    wheelRib->SetLineColor(kBlue);
    //      wheelrib->SetVisibility(1);
    WheelRib->AddVolume(wheelRib);
    TpcSectorAndWheel->AddNodeOverlap(wheelRib,5+i, new TGeoCombiTrans(x, y, z,GetRot("YZXZ"))); 
    TString rotm("R345");
    if (i == 1) rotm = "R015";
    r = 0;
    for (j = 0; j < 4; j ++) {
      if (j == 0) r = TPCG.WheelR2 - 1.5/8.5*(TPCG.WheelR2 - TPCG.WheelR1);
      if (j == 1) r = TPCG.WheelR2 - 6.5/8.5*(TPCG.WheelR2 - TPCG.WheelR1);
      if (j == 2) r = TPCG.WheelR1 - 2.0/8.5*(TPCG.WheelR1 - TPCG.WheelR0);
      if (j == 3) r = TPCG.WheelR1 - 7.0/8.5*(TPCG.WheelR1 - TPCG.WheelR0);
      x = r;
      y = x *tan15*(2*i-1);
      TpcSectorAndWheel->AddNodeOverlap(WheelRibBox, 3+4*i+j, new TGeoCombiTrans(x, y, zWheel1+TPCG.WheelTHK/2, GetRot(rotm))); 
    }
  }
  // RDOs and their cooling
  TGeoVolumeAssembly *TpcRDO = new TGeoVolumeAssembly("TpcRDOAssembly");
  TpcSectorAndWheel->AddNode(TpcRDO, 1, gGeoIdentity);
  Double_t zRDO = TPCG.LengthW/2 + 20.0;
  Double_t RDOCoolingdZ = 2.5;
  Double_t RDOCoolingdY = 1.25;
  Double_t RDOCoolingdX = (38.0 + 9.0 + 58.0)/2;
  TGeoVolume *coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), RDOCoolingdY, RDOCoolingdX, RDOCoolingdZ, +15, 0, 0);
  coolingTube->SetLineColor(kViolet);
  x = TPCG.WheelR2 - RDOCoolingdX;
  y = x*tan15 - RDOCoolingdY ;
  TpcRDO->AddNode(coolingTube,1,new TGeoCombiTrans(x, y, zRDO - 2*dy,GetRot("YZXZ")));
  coolingTube = gGeoManager->MakePara("CoolingTube", GetMed("TPCE_Water_Pipe"), RDOCoolingdY, RDOCoolingdX,RDOCoolingdZ, -15, 0, 0);
  TpcRDO->AddNode(coolingTube,2,new TGeoCombiTrans(x,-y, zRDO - 2*dy,GetRot("YZXZ")));
  coolingTube->SetLineColor(kViolet);
  Double_t dRDOCooling[7] = {16.0, 16.0, 16.0, 16.7, 15.0, 3.5, 14.5};
  Double_t dxRDO = 1.75/2; // A.Lebedev 1 RDO = 5 lb
  Double_t dyRDO = 45./2;
  Double_t dzRDO = 17.0/2;
  TGeoVolume *RDOCard = gGeoManager->MakeBox("RDOCard", GetMed("TPCE_G10"), dxRDO, dyRDO, dzRDO);
  RDOCard->SetLineColor(kYellow); 
  Double_t RCoolingTube = TPCG.WheelR2 - heigTube - 2*dxRDO;
  Int_t iRDOCard = 1;
  for (i = 0; i < 8; i++) {
    if (i > 0) RCoolingTube -= dRDOCooling[i-1];
    dz = heigTube*cm/2;
    dy = widTube*cm/2;
    z  = RCoolingTube;
    dx1 = (z-dz)*tan15 - 2*RDOCoolingdY;
    dx2 = (z+dz)*tan15 - 2*RDOCoolingdY;
    coolingTube = gGeoManager->MakeTrd1("CoolingTube", GetMed("TPCE_Water_Pipe"), dx1, dx2, dy, dz);
    TpcRDO->AddNode(coolingTube,i+3, new TGeoCombiTrans(z, 0, zRDO - 2*dy, GetRot("90XD")));
    coolingTube->SetLineColor(kViolet);
    TpcSectorAndWheel->AddNode(coolingTube,i+3, new TGeoCombiTrans(z, 0, zRDO - 2*dy, GetRot("90XD")));
    if (i != 0 && i != 6) {
      TpcRDO->AddNode(RDOCard, iRDOCard++, new TGeoTranslation(z+dz+dxRDO, 0, zRDO - dzRDO));
    }
  }
  // Cables
  dz = 4.5;
  dy = 4.5/2;
  z  = zRDO + dzRDO + dz;
  x1 = TPCG.WheelR0;
  x2 = TPCG.WheelR2;
  dx = (x2 - x1)/2;
  x  = (x1 + x2)/2;
  TGeoVolumeMulti *Cables = gGeoManager->MakeVolumeMulti("Cables", GetMed("Cables"));
  Cables->SetLineColor(kGreen);
  for (i = 0; i < 2; i++) {
    y  = - (x *tan15 - dy) * (1 - 2*i);
    TGeoVolume *cables = gGeoManager->MakePara("Cables", GetMed("Cables"), dy, dx, dz,-15*(1 - 2*i), 0, 0);
    cables->SetLineColor(kGreen);
    Cables->AddVolume(cables);
    TpcSectorAndWheel->AddNode(cables,i+1, new TGeoCombiTrans( x, y, z,GetRot("YZXZ")));
  }
  // Done
  return TPCE;
}
#endif
