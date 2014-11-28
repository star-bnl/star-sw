// $Id: geometry.h,v 1.10 2009/05/22 20:26:44 fisyak Exp $
// $Log: geometry.h,v $
// Revision 1.10  2009/05/22 20:26:44  fisyak
// Add enum EColor for ROOT Version < 5.22
//
// Revision 1.9  2009/02/25 20:23:19  fisyak
// Add the first version for misalignment
//
// Revision 1.8  2009/01/26 21:55:43  fisyak
// Introduce assembly TpcRefSys for Tpc Reference System
//
// Revision 1.7  2009/01/24 00:20:05  fisyak
// new VolumeId
//
// Revision 1.6  2009/01/14 16:31:53  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.5  2008/08/27 21:48:14  fisyak
// *** empty log message ***
//
// the STAR geometry
#if ROOT_VERSION_CODE < 333312
enum EColor { kWhite =0,   kBlack =1,   kGray=920,
              kRed   =632, kGreen =416, kBlue=600, kYellow=400, kMagenta=616, kCyan=432,
              kOrange=800, kSpring=820, kTeal=840, kAzure =860, kViolet =880, kPink=900 };
#endif
#include "TGeoVolume.h"
#include "GeometryConfiguration.h"
#include "Rotations.h"
#include "Material.h"
#include "Media.h"
#ifdef pipeConfig
  #include "pipe.h"	/* STAR beam pipe mother volume, the geometry  of the STAR beam pipe  */
#endif 	/* pipeConfig */
#ifdef svtConfig
  #include "svt.h"	/* mother of all SVT volumes  */
#endif 	/* svtConfig */
#ifdef pixelConfig
  #include "pixel.h"	/* geometry of the STAR pixel detector  */
#endif 	/* pixelConfig */
#ifdef igtdConfig
  #include "igtd.h"	/* Inner GEM Tracking Detector Geometry  */
#endif 	/* igtdConfig */
#ifdef sconConfig
  #include "scon.h"	/* Silicon tracker supporting cone mother volume, Support structures living before in SVTT moved into CAVE  */
#endif 	/* sconConfig */
#ifdef itspConfig
#include "itsp.h"	/* the Inner Tracker SuPport cone */
#endif 	/* itspConfig */
#ifdef ssdConfig
  #include "ssd.h"       /* Silicon Strip Detector */
#endif /* ssdConfig */
#ifdef fgtdConfig
  #include "fgtd.h"       /*  */
#endif /* fgtdConfig */
#ifdef istConfig
  #include "ist.h"       /* the geometry of the Inner Silicon Tracker  */
#endif /* istConfig */
#ifdef tpcConfig
  #include "tpc.h"	/* TPC envelope  */
#endif 	/* tpcConfig */
#ifdef ftpcConfig
  #include "ftpc.h"	/* FTPC envelope  */
#endif 	/* ftpcConfig */
#ifdef ftroConfig
  #include "ftro.h"	/* other of the single FTPC RO barrel, the geometry of the readout structure of the FTPC */
#endif 	/* ftroConfig */
#ifdef fstdConfig
  #include "fstd.h"	/* mother of one endcap of FSTD, the geometry of the forward silicon tracker pixel detector  */
#endif 	/* fstdConfig */
#ifdef btofConfig
  #include "btof.h"	/* whole CTF system envelope,  the Geometry of Barrel Trigger / Time Of Flight system  */
#endif 	/* btofConfig */
#ifdef ibemConfig
  #include "ibem.h"	/* */
#endif 	/* ibemConfig */
#ifdef calbConfig
  #include "calb.h"	/* EMC Barrel envelope, the geometry of the Barrel EM Calorimeter  */
#endif 	/* calbConfig */
#ifdef richConfig
  #include "rich.h"	/* Ring Image Cerenkov geometry  */
#endif 	/* richConfig */
#ifdef ecalConfig
  #include "ecal.h"	/* */
#endif 	/* ecalConfig */
#ifdef bbcConfig
  #include "bbc.h"	/* the Beam Beam Counter Modules GEOmetry */
#endif 	/* bbcConfig */
#ifdef fpdConfig
  #include "fpd.h"	/* Pb-Glass fpd detector */
#endif 	/* fpdConfig */
#ifdef zcalConfig
  #include "zcal.h"	/* region between the DX and the D0 magnets, the geometry of the Zero deg. Quartz Calorimeter  */
#endif 	/* zcalConfig */
#ifdef magpConfig
  #include "magp.h"	/* magnet mother, the geometry of the STAR magnet  */
#endif 	/* magpConfig */
#ifdef phmdConfig
  #include "phmd.h"	/* the geometry of photon multiplicity detector */
#endif 	/* phmdConfig */
#ifdef upstreamConfig
  #include "upstream.h"	/* upstream mother volume in the STAR cave, the geometry  of the UPSTREAM AreA  */
#endif 	/* upstreamConfig */
#ifdef supportConfig
  #include "support.h"	/* SUPPORT envelope  */
#endif 	/* supportConfig */
#ifdef vpdConfig
  #include "vpd.h"	/* */
#endif 	/* vpdConfig */
#ifdef mutdConfig
  #include "mutd.h"	/* the geometry of the STAR muon trigger system */
#endif 	/* mutdConfig */
#ifdef hpdtConfig
#include "hpdt.h"	/* HPDTGEO geometry of the STAR pixel detector */
#endif 	/* hpdtConfig */
TGeoRotation *rot;
void geometry(const Char_t *tag="y2005g"){
  gSystem->Load("libGeom");
  TGeoManager *star = new TGeoManager(tag,tag);
  star->SetNsegments(80); // number of segments to approximate circles (D 20)
  Rotations();
  Material();
  Media();
  TGeoPcon *pcon = 0;
  TGeoVolume *HALL = gGeoManager->MakePcon("HALL",GetMed("Concrete"),0,360,6); 
  HALL->SetTitle("STAR building");
  pcon = (TGeoPcon*)HALL->GetShape();
  pcon->DefineSection(0,-2000,0,150);
  pcon->DefineSection(1,-1000,0,150);
  pcon->DefineSection(2,-1000,0,500);
  pcon->DefineSection(3, 1000,0,500);
  pcon->DefineSection(4, 1000,0,150);
  pcon->DefineSection(5, 2000,0,150);
  HALL->SetLineColor(2);
  gGeoManager->SetTopVolume(HALL);
  TGeoVolume *CAVE = gGeoManager->MakePcon("CAVE",GetMed("Air"),0,360,6); 
  CAVE->SetTitle("GSTAR cave with subsystem envelopes");
  pcon = (TGeoPcon*)CAVE->GetShape();
  pcon->DefineSection(0,-2000,0,100);
  pcon->DefineSection(1, -950,0,100);
  pcon->DefineSection(2, -950,0,450);
  pcon->DefineSection(3,  950,0,450);
  pcon->DefineSection(4,  950,0,100);
  pcon->DefineSection(5, 2000,0,100);
  CAVE->SetLineColor(2);
  HALL->AddNode(CAVE,1,gGeoIdentity);
  TGeoVolume *PIPE = pipe();
#if defined(complete) || defined(complete) || defined(upgr01) || defined(upgr01) || defined(upgr02) || \
    defined(upgr02) || defined(upgr03) || defined(upgr03) || defined(upgr04) || defined(upgr04) || \
    defined(upgr05) || defined(upgr05) || defined(upgr06) || defined(upgr06) || defined(upgr07) || \
    defined(upgr07) || defined(upgr08) || defined(upgr08) || defined(upgr09) || defined(upgr09) || \
    defined(upgr10) || defined(upgr10) || defined(upgr11) || defined(upgr11) || defined(upgr12) || \
    defined(upgr12) || defined(upgr13) || defined(upgr13) || defined(upgr14) || defined(upgr14)
  CAVE->AddNodeOverlap(PIPE,1,gGeoIdentity);
  CAVE->AddNodeOverlap(PIPE,2,GetRot("180R"));
#else
  CAVE->AddNode(PIPE,1,gGeoIdentity);
  CAVE->AddNode(PIPE,2,GetRot("180R"));
#endif
  TGeoVolumeAssembly *TpcRefSys = new TGeoVolumeAssembly("TpcRefSys");
  CAVE->AddNode(TpcRefSys,1);
#ifdef svtConfig
  TGeoVolume *SVTT = svt();
  #if defined(complete) || defined(upgr02)
  TpcRefSys->AddNodeOverlap(SVTT,1,gGeoIdentity);
  #else
  TpcRefSys->AddNode(SVTT,1,gGeoIdentity);
  #endif
#endif 	/* svtConfig */
#ifdef sconConfig
  TGeoVolume *SCON = scon();
  SVTT->AddNode(SCON,1,gGeoIdentity);
  SVTT->AddNode(SCON,2,GetRot("180R"));
#endif 	/* sconConfig */
#ifdef itspConfig
  TGeoVolume *SCON = itsp();
  TpcRefSys->AddNode(SCON,1,gGeoIdentity);
#endif 	/* sconConfig */

#ifdef ssdConfig
  TGeoVolume *SFMO = ssd();
  #if defined(upgr01) || defined(upgr03) || defined(upgr04) || defined(upgr05) || defined(upgr06) || \
      defined(upgr07) || defined(upgr10) || defined(upgr11) || defined(upgr12) || defined(upgr13) || \
      defined(upgr14) || defined(upgr15)
  TpcRefSys->AddNode(SFMO,1,gGeoIdentity);  
  #else
  SVTT->AddNodeOverlap(SFMO,1,gGeoIdentity);
  #endif /* upgr */
#endif 	/* ssdConfig */
#ifdef tpcConfig
  TGeoVolume *TPCE = tpc();
  TpcRefSys->AddNode(TPCE,1,gGeoIdentity);
#endif 	/* tpcConfig */
#ifdef ftpcConfig
  TGeoVolume *FTPC = ftpc();
  #if ! defined(svtConfig) 
  TpcRefSys->AddNodeOverlap(FTPC,1,new TGeoTranslation(0,0,209.5));
  TpcRefSys->AddNodeOverlap(FTPC,2,new TGeoCombiTrans(0,0,-209.5,GetRot("180R")));
  #else
  SVTT->AddNodeOverlap(FTPC,1,new TGeoTranslation(0,0,209.5));
  SVTT->AddNodeOverlap(FTPC,2,new TGeoCombiTrans(0,0,-209.5,GetRot("180R")));
  #endif
#endif 	/* ftpcConfig */
#ifdef ftroConfig /* the geometry of the readout structure of the FTPC */
  TGeoVolume *FTMO = ftro();
  TpcRefSys->AddNode(FTMO,1,new TGeoTranslation(0,0,309));
  TpcRefSys->AddNode(FTMO,2,new TGeoCombiTrans(0,0,-309,GetRot("R180")));
#endif 	/* ftroConfig */
#ifdef fstdConfig  /* the geometry of the forward silicon tracker pixel detector */
  TGeoVolume *FSMO = fstd();
  TpcRefSys->AddNodeOverlap(FSMO,1,new TGeoTranslation(0,0,32.5));
#endif 	/* fstdConfig */
#ifdef btofConfig /* the Geometry of Barrel Trigger / Time Of Flight system */
  TGeoVolume *BTOF = btof();
  TpcRefSys->AddNode(BTOF,1,gGeoIdentity);
#endif 	/* btofConfig */
#ifdef calbConfig /* EMC Barrel envelope */
  TGeoVolume *CALB = calb();
  CAVE->AddNode(CALB,1,gGeoIdentity);
#endif 	/* calbConfig */
#ifdef richConfig /* Ring Image Cerenkov geometry */
  TGeoVolume *RICH = rich();
  CALB->AddNode(RICH,1,new TGeoCombiTrans(116.253,-200.8902,-0.3118184,rot));
#endif 	/* richConfig */
#ifdef ecalConfig
  TGeoVolume *ECAL = ecal();
  CAVE->AddNode(ECAL,1,new TGeoTranslation(0,0,289.385));
  #if defined(complete) || defined(y2003x)
  CAVE->AddNode(ECAL,2,new TGeoCombiTrans(0,0,-289.385,GetRot("180R")));
  #endif
#endif 	/* ecalConfig */
#ifdef bbcConfig /* the Beam Beam Counter Modules GEOmetry */
  TGeoVolume *BBC = bbc();
  CAVE->AddNode(BBC,1);
#endif 	/* bbcConfig */
#ifdef fpdConfig /* Pb-Glass fpd detector */
  fpd(CAVE);
#endif 	/* fpdConfig */
#ifdef zcalConfig /* region between the DX and the D0 magnets, the geometry of the Zero deg. Quartz Calorimeter */
  TGeoVolume *ZCAL = zcal();
  CAVE->AddNode(ZCAL,1,new TGeoTranslation(0,0,1767.66));
  CAVE->AddNode(ZCAL,2,new TGeoCombiTrans(0,0,-1767.66,GetRot("180R")));
#endif 	/* zcalConfig */
#ifdef magpConfig /* the geometry of the STAR magnet */
  TGeoVolume *MAGP = magp();
  CAVE->AddNode(MAGP,1,gGeoIdentity);
#endif 	/* magpConfig */
#ifdef mutdConfig /* the geometry of the STAR muon trigger system */
  TGeoVolume *MUTD = mutd();
  CAVE->AddNode(MUTD,1,gGeoIdentity);
#endif 	/* mutdConfig */
#ifdef pixelConfig /* geometry of the STAR pixel detector */
  TGeoVolume *PXMO = pixel();
  #ifdef defined(complete)
  SVTT->AddNode(PXMO,1,gGeoIdentity);
  #elif defined(upgr01) || defined(upgr03) || defined(upgr05) || defined(upgr06) || defined(upgr07) ||\
        defined(upgr08) || defined(upgr09) || defined(upgr10) || defined(upgr11) || defined(upgr12) ||\
        defined(upgr13) || defined(upgr14) || defined(upgr15)
  TpcRefSys->AddNode(PXMO,1,gGeoIdentity);
  #else
  TpcRefSys->AddNodeOverlap(PXMO,1,gGeoIdentity);
  #endif
  #if pixelConfig == 2 || pixelConfig == 3 || pixelConfig == 5 || pixelConfig == 6
    #if pixelConfig == 2 || pixelConfig == 5 || pixelConfig == 6 
  TGeoVolume *PXBX = gGeoManager->MakeTube("PXBX",GetMed("PIXL_BERILLIUM"),8.5,8.6,24); PXBX->SetTitle("the exoskeleton of the beampipe");
    #else /* pixelConfig == 3 */
  TGeoVolume *PXBX = gGeoManager->MakeTube("PXBX",GetMed("PIXL_BERILLIUM"),5.9,6,24); PXBX->SetTitle("the exoskeleton of the beampipe");
    #endif
  PXBX->SetLineColor(3);
  CAVE->AddNode(PXBX,1,gGeoIdentity);
  #endif
#endif 	/* pixelConfig */
#ifdef igtdConfig /* Inner GEM Tracking Detector Geometry */
  TGeoVolume *IGMO = igtd();
  TpcRefSys->AddNodeOverlap(IGMO,1,new TGeoTranslation(0,0,100.95));
#endif 	/* igtdConfig */
#ifdef hpdtConfig
  TGeoVolume *YPXM = hpdt();
  TpcRefSys->AddNode(YPXM,1,gGeoIdentity);
#endif 	/* hpdtConfig */
#ifdef fgtdConfig
  TGeoVolume *FGMO = fgtd();
  TpcRefSys->AddNodeOverlap(FGMO,1,new TGeoTranslation(0,0,105.9));
#endif 	/* fgtdConfig */
#ifdef istConfig /* the geometry of the Inner Silicon Tracker */
  TGeoVolume *IBMO = ist();
  TpcRefSys->AddNode(IBMO,1,gGeoIdentity);
#endif 	/* istConfig */
#ifdef phmdConfig /* the geometry of photon multiplicity detector */
  TGeoVolume *PHMD = phmd();
  CAVE->AddNode(PHMD,1,new TGeoTranslation(0,0,-539));
#endif 	/* phmdConfig */
#ifdef upstreamConfig
  TGeoVolume *UPST = upstream();
  CAVE->AddNodeOverlap(UPST,1,new TGeoTranslation(0,0,1131.83));
  CAVE->AddNodeOverlap(UPST,2,new TGeoCombiTrans(0,0,-1131.83,GetRot("180R")));
#endif 	/* upstreamConfig */
#ifdef supportConfig
  TGeoVolume *SUPO = support();
  #if defined(year2002) || defined(year2003) || defined(year_2a) || defined(year_2b)
  CAVE->AddNodeOverlap(SUPO,1,new TGeoTranslation(0,0,27.82));
  CAVE->AddNodeOverlap(SUPO,2,new TGeoCombiTrans(0,0,-27.82,GetRot("180R")));
  #else
  CAVE->AddNodeOverlap(SUPO,1,new TGeoTranslation(0,0,-245.195));
  CAVE->AddNodeOverlap(SUPO,2,new TGeoCombiTrans(0,0,245.195,GetRot("180R")));
  #endif
#endif 	/* supportConfig */
#ifdef vpdConfig
  TGeoVolume *VPDD = vpd();
  #if defined(complete) || defined(upgr01) || defined(upgr03) || defined(upgr04) || defined(upgr05) ||\
      defined(upgr06) || defined(upgr07) || defined(upgr08) || defined(upgr09) || defined(upgr10) || \
      defined(upgr11) || defined(upgr12) || defined(upgr13) || defined(upgr14) || defined(upgr15) || \
      defined(year2001) || defined(year_2a) || defined(year_2b)
  CAVE->AddNodeOverlap(VPDD,1,new TGeoTranslation(0,0,550));
  CAVE->AddNodeOverlap(VPDD,2,new TGeoCombiTrans(0,0,-550,GetRot("180R")));
  #elif defined(year2002)
  CAVE->AddNodeOverlap(VPDD,1,new TGeoTranslation(0,0,563.1688));
  CAVE->AddNodeOverlap(VPDD,2,new TGeoCombiTrans(0,0,-561.2638,GetRot("180R")));
  #elif defined(y2003a) || defined(y2003b) || defined(y2003c) || defined(y2003x) || defined(year2003)
  CAVE->AddNodeOverlap(VPDD,1,new TGeoTranslation(0,0,563.4069));
  CAVE->AddNodeOverlap(VPDD,2,new TGeoCombiTrans(0,0,-564.4388,GetRot("180R")));
  #elif defined(upgr20) || defined(upgr21) || defined(y2007) || defined(y2007a) || defined(y2007g) || defined(y2008)
  CAVE->AddNodeOverlap(VPDD,1,new TGeoTranslation(0,0,571.45));
  CAVE->AddNodeOverlap(VPDD,2,new TGeoCombiTrans(0,0,-571.45,GetRot("180R")));
  #else 
  /* defined(dev2005) || defined(upgr02) || defined(y2004) || defined(y2004a) || defined(y2004b) || 
     defined(y2004c) || defined(y2004d) || defined(y2004x) || defined(y2004y) || defined(y2005) || 
     defined(y2005b) || defined(y2005c) || defined(y2005d) || defined(y2005e) || defined(y2005f) || 
     defined(y2005g) || defined(y2005x) || defined(y2006) || defined(y2006a) || defined(y2006b) || 
     defined(y2006c) || defined(y2006g) */
  CAVE->AddNodeOverlap(VPDD,1,new TGeoTranslation(0,0,573.5669));
  CAVE->AddNodeOverlap(VPDD,2,new TGeoCombiTrans(0,0,-574.5688,GetRot("180R")));
  #endif
  #ifdef ibemConfig
  ibem(CAVE,VPDD);
  #endif 	/* ibemConfig */
#endif 	/* vpdConfig */
  gGeoManager->CloseGeometry(); 
}

