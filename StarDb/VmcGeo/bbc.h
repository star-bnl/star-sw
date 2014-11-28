// $Id: bbc.h,v 1.4 2009/01/24 00:20:05 fisyak Exp $
// $Log: bbc.h,v $
// Revision 1.4  2009/01/24 00:20:05  fisyak
// new VolumeId
//
// Revision 1.3  2008/08/27 21:48:12  fisyak
// *** empty log message ***
//
// the Beam Beam Counter Modules GEOmetry
#ifdef bbcConfig
#if !defined(__CINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "Rotations.h"
#include "Material.h"
#include "Media.h"
#include "CreateGeometry.h"
#include "TGeoMatrix.h"
#include "TGeoPgon.h"
#include "TGeoPara.h"
#include "TGeoCompositeShape.h"
#endif
TGeoVolume *bbc() {// the Beam Beam Counter Modules GEOmetry
  struct	BBCG_t {
    Int_t     version; // Version = Geometry version							      
    Double_t *zdis;    // zdis    =z-coord from center in STAR (715/2+6*2.54+1=373.8)                      
  };
  Double_t   zdis[2]  = {374.24,-374.24};
  BBCG_t BBCG = { // BBC geometry
    1		, // Version = Geometry version							      
    &zdis[0]      // zdis    =z-coord from center in STAR (715/2+6*2.54+1=373.8)                      
  };
  struct	HEXG_t {
    Int_t    type;    // Type	= 1 for small hex tile, 2 for large tile	     
    Double_t irad;    // irad	= inscribing circle radius =9.64/2*sin(60)=4.174     
    Double_t clad;    // clad	= cladding thickness				     
    Double_t thick;   // thick	= thickness of tile				     
    Double_t zoffset; // zoffset	= z-offset from center of BBCW (1), or BBCE (2)	     
    Double_t xoffset; // xoffset	= x-offset center from beam for BBCW (1), or BBCE (2)
    Double_t yoffset; // yoffset	= y-offset center from beam for BBCW (1), or BBCE (2)
  };
  HEXG_t HEXG[2] = {       // hexagon tile geometry
    {1	        , // Type	= 1 for small hex tile, 2 for large tile	     
     4.174	, // irad	= inscribing circle radius =9.64/2*sin(60)=4.174     
     0.1	, // clad	= cladding thickness				     
     1.0	, // thick	= thickness of tile				     
     1.5	, // zoffset	= z-offset from center of BBCW (1), or BBCE (2)	     
     0.0	, // xoffset	= x-offset center from beam for BBCW (1), or BBCE (2)
     0.0       }, // yoffset	= y-offset center from beam for BBCW (1), or BBCE (2)
    {2	        , // Type	= 1 for small hex tile, 2 for large tile
     16.697     , // irad	= inscribing circle radius (4x that of small one)
     0.1	, // clad	= cladding of tile
     1.0	, // thick	= thickness of tile
     -1.5	, // zoffset	= z-offset from center of BBCW (1), or BBCE (2)
     0.0	, // xoffset	= x-offset center from beam for BBCW (1), or BBCE (2)
     0.0	} // yoffset	= y-offset center from beam for BBCW (1), or BBCE (2)
  };
  Double_t   actr,srad,lrad,ztotal,x0,y0,theta0,phi0;
  Double_t   xtrip,ytrip,rtrip,thetrip,rsing,thesing;
  Int_t	     I_trip,J_sing, type;
  Double_t rmin,rmax, dz;  
  Double_t x,y,z;
  Double_t pi = TMath::Pi();

  TGeoVolumeAssembly *BBC = new TGeoVolumeAssembly("BBC");
  rmin = HEXG[0].irad;
  rmax = HEXG[1].irad*6.0;
  ztotal = 
    HEXG[0].thick+2*TMath::Abs(HEXG[0].zoffset) +
    HEXG[1].thick+2*TMath::Abs(HEXG[1].zoffset);
  dz=ztotal/2; 
  TGeoVolume *BBCM = gGeoManager->MakeTube("BBCM",GetMed("BBCM_STANDARD"),rmin,rmax,dz); 
  BBCM->SetTitle("one BBC East or West");
  BBCM->SetVisibility(0);
  BBCM->SetLineColor(kBlue);
  BBC->AddNode(BBCM,1,new TGeoTranslation(0,0,zdis[0]));
  BBC->AddNode(BBCM,2,new TGeoCombiTrans( 0,0,zdis[1],GetRot("P270")));

  TGeoVolumeMulti *BBCA = gGeoManager->MakeVolumeMulti("BBCA",GetMed("BBCM_STANDARD"));
  BBCA->SetTitle("one BBC Annulus");
  BBCA->SetVisibility(0);
  BBCA->SetLineColor(kGreen);
  TGeoVolumeMulti *THXM = gGeoManager->MakeVolumeMulti("THXM",GetMed("BBCM_STANDARD"));
  THXM->SetTitle("on Triple HeXagonal");
  THXM->SetVisibility(0);
  THXM->SetLineColor(2);
  TGeoVolumeMulti *SHXT = gGeoManager->MakeVolumeMulti("SHXT",GetMed("BBCM_STANDARD"));
  SHXT->SetTitle("one Single HeXagonal Tile");
  SHXT->SetLineColor(kPink);
  TGeoVolumeMulti *CLAD = gGeoManager->MakeVolumeMulti("CLAD",GetMed("BBCM_ALKAP"));
  CLAD->SetTitle("one CLADding of BPOL active region");
  CLAD->SetLineColor(kGreen);
  TGeoVolumeMulti *BPOL = gGeoManager->MakeVolumeMulti("BPOL",GetMed("BBCM_CPOLYSTYREN"));
  BPOL->SetTitle("one Bbc POLystyren active scintillator layer");
  
  for (type = 1; type <= 2; type++) {
    dz=HEXG[type-1].thick/2; rmin=HEXG[type-1].irad; rmax=HEXG[type-1].irad*6.0;
    TGeoVolume *bbca = gGeoManager->MakeTube("BBCA",GetMed("BBCM_STANDARD"),rmin,rmax,dz);
    bbca->SetTitle("one BBC Annulus");
    bbca->SetVisibility(0);
    bbca->SetLineColor(kGreen);
    BBCA->AddVolume(bbca);
    z=HEXG[type-1].zoffset; x=HEXG[type-1].xoffset; y=HEXG[type-1].yoffset;
    BBCM->AddNode(bbca,type,new TGeoTranslation(x,y,z));
    
    x0=HEXG[type-1].irad*TMath::Tan(pi/6.0);
    y0=HEXG[type-1].irad*3.0;
    rtrip = TMath::Sqrt(x0*x0+y0*y0);
    theta0 = TMath::ATan(y0/x0);
    
    dz=HEXG[type-1].thick/2; rmax=HEXG[type-1].irad*2.0/TMath::Sin(pi/3.0);
    TGeoVolume *thxm = gGeoManager->MakeTube("THXM",GetMed("BBCM_STANDARD"),0, rmax, dz); 
    thxm->SetTitle("on Triple HeXagonal");
    thxm->SetVisibility(0);
    thxm->SetLineColor(2);
    THXM->AddVolume(thxm);
    for (I_trip =0; I_trip <= 5; I_trip++) {
      phi0 = I_trip*60;
      thetrip = theta0+I_trip*pi/3.0;
      xtrip = rtrip*TMath::Cos(thetrip);
      ytrip = rtrip*TMath::Sin(thetrip);
      bbca->AddNodeOverlap(thxm,I_trip+1,new TGeoCombiTrans(xtrip,ytrip,0,GetRot(Form("R%03.0f",phi0))));
    };
    
    TGeoVolume *shxt = gGeoManager->MakePgon("SHXT",GetMed("BBCM_STANDARD"),0,360,6,2); 
    ((TGeoPcon*)shxt->GetShape())->DefineSection(0,-HEXG[type-1].thick/2,0,HEXG[type-1].irad);
    ((TGeoPcon*)shxt->GetShape())->DefineSection(1, HEXG[type-1].thick/2,0,HEXG[type-1].irad);
    shxt->SetTitle("one Single HeXagonal Tile");
    shxt->SetLineColor(kPink);
    SHXT->AddVolume(shxt);
    
    for (J_sing = 0; J_sing <= 2; J_sing++) {
      rsing=HEXG[type-1].irad/TMath::Sin(pi/3.0);
      thesing=J_sing*pi*2.0/3.0;
      z=0; x=rsing*TMath::Cos(thesing); y=rsing*TMath::Sin(thesing);
      thxm->AddNode(shxt,J_sing+1,new TGeoTranslation(x,y,z));
    }
    
    actr = HEXG[type-1].irad-HEXG[type-1].clad;
    TGeoVolume *clad = gGeoManager->MakePgon("CLAD",GetMed("BBCM_ALKAP"),0,360,6,2); 
    clad->SetTitle("one CLADding of BPOL active region");
    ((TGeoPcon*)clad->GetShape())->DefineSection(0,-HEXG[type-1].thick/2,actr,HEXG[type-1].irad);
    ((TGeoPcon*)clad->GetShape())->DefineSection(1, HEXG[type-1].thick/2,actr,HEXG[type-1].irad);
    clad->SetLineColor(kGreen);
    CLAD->AddVolume(clad);
    shxt->AddNode(clad,1,gGeoIdentity);
      
    TGeoVolume *bpol = gGeoManager->MakePgon("BPOL",GetMed("BBCM_CPOLYSTYREN"),0,360,6,2); 
    bpol->SetTitle("one Bbc POLystyren active scintillator layer");
    ((TGeoPcon*)bpol->GetShape())->DefineSection(0,-HEXG[type-1].thick/2,0,actr);
    ((TGeoPcon*)bpol->GetShape())->DefineSection(1, HEXG[type-1].thick/2,0,actr);
    bpol->SetLineColor(kBlue);
    BPOL->AddVolume(bpol);
    shxt->AddNode(bpol,1,gGeoIdentity);
    /* define Birks law parameters
       Call GSTPAR (ag_imed,'BIRK1',1.)
       Call GSTPAR (ag_imed,'BIRK2',0.013)
       Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
    */     
  }
   return BBC;
}
#endif
