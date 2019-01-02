//#define __NOVMC__
#include "StiVMCToolKit.h" 
#ifdef __ROOT__
#include "StMaker.h"
#endif
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <string.h>
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TFile.h"
#include "StMessMgr.h"
#else
#define BIT(n)       (1 << (n))
#endif
static Int_t __addelement__(Int_t NElem,const TGeoMaterial *mat, Elem_t *ElementList) {
   return StiVMCToolKit::Add2ElementList(NElem, mat, ElementList);
}

struct Elem_t {
  Int_t    index;
  Double_t W;
  Double_t A;
  Double_t Z;
  Elem_t() : index(0),W(0),A(0),Z(0) {}
};
struct ElemV_t {
  const Char_t   *name;
  Double_t     A;
  Double_t     Z;
  Double_t     V; // by  volume 
  Double_t     W;
};
static Int_t m_Debug = 0;
static const Int_t NoElemMax = 10000;
static VolumeMap_t VolumesToBeAveraged[] = { 
  {"PIPE","the STAR beam pipe mother volume","HALL_1/CAVE_1/PIPE_1-2/*","",""},
  {"PIPC","the Central Beam PIPe Volum","HALL_1/CAVE_1/PIPE_1-2/PIPC_1/*","",""},
  {"PVAC","the Vacuum Volume of Be section of pipe","HALL_1/CAVE_1/PIPE_1-2/PIPC_1/PVAC_1","",""},
  {"PIPO","Steel pipe from Be to 1st flanges","HALL_1/CAVE_1/PIPE_1-2/PIPO_1/PVAO_1","",""},
  {"PVAO","its cavity","HALL_1/CAVE_1/PIPE_1-2/PIPO_1/PVAO_1","",""},
  // Pixel
  {"PXBX","Extrenal Berillium tube","HALL_1/CAVE_1/PXBX_1","",""},
  // SVT
  {"SOUM", "Outer shileding structure","HALL_1/CAVE_1/SVTT_1/SOUM_1/*","",""},
  {"SXRL", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXRL_1-2/*","",""}, 
  {"SXR1", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXR1_3-4/*","",""}, 
  {"SXR2", "Circular water feeds","HALL_1/CAVE_1/SVTT_1/SXR2_5-6/*","",""},
  {"SCBM", "Mother of All Cables","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/*","",""},
  {"SCBL", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCBL_1","",""},
  {"SCB1", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB1_2","",""},
  {"SCB2", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB2_3","",""},
  {"SCB3", "The bundles of cables connecting PCBs with the transition boards","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SCB3_4","",""},
  {"SFED", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFED_1","",""},
  {"SFE1", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFE1_2","",""},
  {"SFE2", "bundles of water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SFE2_3","",""},
  {"SPLS", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPLS_1","",""},
  {"SPL1", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPL1_2","",""},
  {"SPL2", "plastic of the water pipes","HALL_1/CAVE_1/SVTT_1/SCBM_1-2/SPL2_3","",""},
  {"SALM", "aluminum shield mesh","HALL_1/CAVE_1/SVTT_1/SALM_1-2","",""},
  {"SOSH", "SVT outer shield","HALL_1/CAVE_1/SVTT_1/SOSH_1","",""},
  {"SISH", "SVT inner shield","HALL_1/CAVE_1/SVTT_1/SISH_1","",""},
  {"SLYD", "layer mother","HALL_1/CAVE_1/SVTT_1/SLYD_1/*","",""},
  {"SLY1", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY1_2/*","",""},
  {"SLY2", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY2_3/*","",""},
  {"SLY3", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY3_4/*","",""},
  {"SLY4", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY4_5/*","",""},
  {"SLY5", "layer mother","HALL_1/CAVE_1/SVTT_1/SLY5_6/*","",""},
  {"SVTD", "an active wafer volume","HALL_1/CAVE_1/SVTT_1/SLY*/SLS*/SLD*/STL*/STS*/SVTD_1","svt","SVTD"}, // <+++
  {"SROD", "Support rod","HALL_1/CAVE_1/SVTT_1/SROD_1-2","",""},
  {"SBSP", "Beampipe support mother","HALL_1/CAVE_1/SVTT_1/SBSP_1-2","",""},
  //  {"SCON", "Support cone mother","HALL_1/CAVE_1/SVTT_1/SCON_1-2/*","",""},
  {"SBWC", "water manifold to support cone bracket mother","HALL_1/CAVE_1/SVTT_1/SBWC_1-2/*","",""},
  {"SWMM", "water manifold mother","HALL_1/CAVE_1/SVTT_1/SWMM_1-2/*","",""},
  {"SIES", "Volume to hold inner endring screws","HALL_1/CAVE_1/SVTT_1/SIES_1-2/*","",""},
  {"SOES", "Volume to hold outer endring screws","HALL_1/CAVE_1/SVTT_1/SOES_1-2/*","",""},
  {"SBRG", "Bracket joining the end rungs","HALL_1/CAVE_1/SVTT_1/SBRG_1-2/*","",""},
  {"SOER", "outer end ring","HALL_1/CAVE_1/SVTT_1/SOER_1-2/*","",""},
  {"SIRT", "inner end ring tube piece ","HALL_1/CAVE_1/SVTT_1/SIRT_1-2","",""},
  {"SIRP", "inner end ring polygon piece ","HALL_1/CAVE_1/SVTT_1/SIRP_1-2","",""},
  {"STAC", "twinax cable approximation, copper","HALL_1/CAVE_1/SVTT_1/SCON_1/STAC_1-2","",""},
  // SSD
  //  {"SFMO", "the mother of all Silicon Strip Detector volumes","HALL_1/CAVE_1/SVTT_1/SFMO_1","",""},
  {"SCMP","SSD mounting plate inserted in the cone","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCMP_1-8","",""},
  {"SCVM","SSD V-shape mouting piece","HALL_1/CAVE_1/SVTT_1/SFMO_1/SCVM_1-8/*","",""},
  {"SSLT","the linking (sector to the cone) tube","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLT_1-8","",""},
  {"SSLB","the linking (sector to the cone)","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSLB_1-8","",""},
  {"SSRS","the side of the small rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRS_1-4","",""},
  {"SSRT","the top of the side rib","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSRT_1-4","",""},
  {"SSSS","Side parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSSS_1-4","",""},
  {"SSST","Top parts of the small sectors","HALL_1/CAVE_1/SVTT_1/SFMO_1/SSST_1-4","",""},
  //  {"SFLM","the mother of the ladder","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/*","",""}, 
  {"SFSM","the structure mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFSM_1/*","",""},
  {"SFDM","the detectors and adcs mother volume","HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/*","",""},
  {"SFSD","the strip detector",      "HALL_1/CAVE_1/SVTT_1/SFMO_1/SFLM_1-20/SFDM_1/SFSW_1-16/SFSD_1","ssd",""},// <+++
  // TPC
  //  {"TPCE","the TPC system in STAR","HALL_1/CAVE_1/TPCE_1","",""},
  {"TPCW","the TPC supporting endcap Wheel","HALL_1/CAVE_1/TPCE_1/TPCW_1-2/*","",""},
  {"TPEA","one endcap placed in TPC","HALL_1/CAVE_1/TPCE_1/TPEA_1-2/*","",""},
  {"TPCM","the Central Membrane placed in TPC","HALL_1/CAVE_1/TPCE_1/TPCM_1","",""},
  {"TOFC","defines outer field cage - fill it with insulating gas already","HALL_1/CAVE_1/TPCE_1/TOFC_1/*","",""},
  {"TIFC","defines the Inner Field Cage placed in TPC","HALL_1/CAVE_1/TPCE_1/TIFC_1/*","",""},
  {"TPGV","the Gas Volume placed in TPC","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/*","",""},
  {"TPSS","a division of gas volume corresponding to a supersectors","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/TPSS_1-12/*","",""},
  {"TPAD","(inner) real padrow with dimensions defined at positioning time","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/TPSS_1-12/TPAD_1-39","tpc",""},// <+++
  {"TPA1","(outer) real padrow with dimensions defined at positioning time","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/TPSS_1-12/TPA1_40-73","tpc",""}
};
static Int_t NofVolToBEAveraged = sizeof(VolumesToBeAveraged)/sizeof(VolumeMap_t);
static VolumeMap_t TopVolumes[] = {// Mother volume and sensitive detectors
  {"BBCM","Beam Beam Counter Modules Geometry","","",""},
  {"BTOF","the whole CTF system envelope","","",""},
  {"CALB","the geometry of the Barrel EM Calorimeter","","",""},
  {"ECAL","the EM EndCap Calorimeter GEOmetry","","",""},
  {"FBOX","one Pb-Glass fpd detector","","",""},
  {"FBO1","an other Pb-Glass fpd detector","","",""},
  {"FTMO","the mother of the single FTPC RO barrel","","",""},
  {"IBEM","the IBeam structure beneath the Bell reducer cone","","",""},
  {"MAGP","the geometry of the STAR magnet","","",""},
  {"PHMD","the Photon Multiplicity Detector","","",""},
  {"SUPO","the geometry of the Forward TPC supports in STAR","","",""},

  {"SVTT","the SVT geometry for STAR","","",""},

  {"SFMO","the mother of all Silicon Strip Detector volumes (inside of SVTT)","","",""},
  
  {"FTPC","the geometry of the Forward TPC in STAR (inside of SVTT)","","",""},

  {"UPST","the geometry  of the UPSTREAM AreA.","","",""},
  {"VPDD","the Pseudo Vertex Position Detector of STAR","","",""},
  {"ZCAL","the geometry of the Zero deg. Quartz Calorimeter","","",""}
};
static Int_t nTopVol = sizeof(TopVolumes)/sizeof(VolumeMap_t);
  /*
    "Air"                    4    1.214e-3        #. Huhtinen, Air 18 degr.C and 58% humidity
    "Nitrogen"         74.94            #. Weight fraction
    "Oxygen"           23.69            #. Weight fraction
    "Argon"             1.29            #. Weight fraction
    "Hydrogen"          0.08            #. Weight fraction
  */
static ElemV_t Air[] = {
  {"N_2", 14.00674, 7, 2*78.084, 0},
  {"O_2", 15.9994 , 8, 2*(20.946 + 0.033), 0},//    {"CO_2",  0, 0, 2*0.033, 0}, 2*(20.946 + 0.033), 0},
  {"C",   12.011, 6,  0.033, 0},
  {"Ar",  39.948, 18, 0.934, 0}
};
static Int_t NAir = 0;
static ElemV_t P10[] = {
  {"Ar",  40,18,0.95744680,0},
  {"C" ,  12, 6,0.03191489,0},
  {"H" ,   1, 1,0.01063830,0}
};
static Int_t NP10 = 3;
// {"Ne", 20.1797, 10, 18.18e-6, 0},
// {"He",  4.002602, 2, 5.24e-6, 0},
// {"Kr", 0., 0, 1.14e-6, 0}
// {"Xe", 0., 0, 0.087e-6, 0}
// {H_2", 0., 0, 0.5e-6}
// {"CH_4",0., 0, 2e-6}
// {N_2 O", 0., 0, 0.5-e6}
//________________________________________________________________________________
void StiVMCToolKit::SetDebug(Int_t m) {m_Debug = m;}
//________________________________________________________________________________
Int_t StiVMCToolKit::Debug() {return m_Debug;}
//________________________________________________________________________________
void StiVMCToolKit::PrintShape(TGeoShape *shape) {
  TGeoBBox *box = 0, *Box = 0;
  TGeoTrd1 *trd1 = 0;
  TGeoTrd2 *trd2 = 0;
  TGeoTube *tube = 0;
  TGeoTubeSeg *tubs = 0;
  TGeoPcon *pcon = 0;
  TGeoPgon *pgon = 0;
  TGeoCone *cone = 0;
  TGeoConeSeg *cons = 0;
  TGeoArb8 *arb8 = 0;
  TGeoEltu *eltu = 0;
  Double_t *XY;
  Double_t dZ;
  //  Double_t paramsBB[3];
  Double_t paramsBC[4];
  Int_t i, j;
  Int_t Nz = 0;
  shape->GetBoundingCylinder(paramsBC);
  shape->ComputeBBox();
  Box = (TGeoBBox *) shape;
  for (Int_t bit = 24; bit >= 9; bit--) {//cout << bit << "\t"; 
    if (shape->TestShapeBit(BIT(bit))) {
      switch (BIT(bit)) {
      case TGeoShape::kGeoBox: 
	box = (TGeoBBox *) shape;  
	cout << "Box \tdX\t" << box->GetDX() << "\tdY\t" <<  box->GetDY() << "\tdZ\t" <<  box->GetDZ() 
	     << endl; 
	break;
      case TGeoShape::kGeoTrd1:
	trd1 = (TGeoTrd1 *) shape; 
	cout << "Trd1\tdX1\t" << trd1->GetDx1() << "\tdX2\t" << trd1->GetDx2()
	     << "\tdY\t" <<  trd1->GetDy() << "\tdZ\t" <<  trd1->GetDz() 
	     << endl; 
	break;   
      case TGeoShape::kGeoTrd2:
	trd2 = (TGeoTrd2 *) shape; 
	cout << "Trd2\tdX1\t" << trd2->GetDx1() << "\tdX2\t" << trd2->GetDx2()
	     << "\tdY1\t" <<  trd2->GetDy1() << "\tdY2\t" <<  trd2->GetDy2() 
	     << "\tdZ\t" <<  trd2->GetDz() << endl; 
	break;   
      case TGeoShape::kGeoTubeSeg:  
	tubs = (TGeoTubeSeg *) shape; 
	cout << "Tubs\tRmin\t" << tubs->GetRmin() << "\tRmax\t" <<  tubs->GetRmax() << "\tdZ\t" << tubs->GetDz() 
	     << "\tPhi1\t" << tubs->GetPhi1() << "\tPhi2\t" << tubs->GetPhi2()
	     << endl;
	break;
      case TGeoShape::kGeoTube:
	tube = (TGeoTube *) shape; 
	cout << "Tube\tRmin\t" << tube->GetRmin() << "\tRmax\t" <<  tube->GetRmax() << "\tdZ\t" << tube->GetDz() 
	     << endl;
	break;
      case TGeoShape::kGeoPcon:    
	pcon = (TGeoPcon *) shape;
	Nz = pcon->GetNz();
	cout << "Pcon" 
	     << "\tPhi1\t" << pcon->GetPhi1() << "\tDphi\t" << pcon->GetDphi() << "\tNz\t" << Nz << endl;
	for (i = 0; i < Nz; i++) {
	  cout << i << "\tZ\t" << pcon->GetZ(i) << "\tRmin\t" << pcon->GetRmin(i) << "\tRmax\t" << pcon->GetRmax(i) << endl;
	}
	cout << endl;
	break;
      case TGeoShape::kGeoPgon:    
	pgon = (TGeoPgon *) shape;
	Nz = pgon->GetNz();
	//	      pcon = (TGeoPcon *) shape;
	cout << "Pgon\tNedges\t" << pgon->GetNedges() 
	     << "\tPhi1\t" << pgon->GetPhi1() << "\tDphi\t" << pgon->GetDphi() << "\tNz\t" <<Nz << endl;
	for (i = 0; i <Nz; i++) {
	  cout << i << "\tZ\t" << pgon->GetZ(i) << "\tRmin\t" << pgon->GetRmin(i) << "\tRmax\t" << pgon->GetRmax(i) << endl;
	}
	cout << endl;
	break;
      case TGeoShape::kGeoCone:    
	cone = (TGeoCone *) shape;
	cout << "Cone\tdZ\t" << cone->GetDz() 
	     << "\tRmin1\t" << cone->GetRmin1() << "\tRmax1\t" << cone->GetRmax1()  
	     << "\tRmin2\t" << cone->GetRmin2() << "\tRmax2\t" << cone->GetRmax2() 
	     << endl;
	break;
      case TGeoShape::kGeoConeSeg: 
	cons = (TGeoConeSeg *) shape;
	cout << "Cons\tdZ\t" << cons->GetDz() 
	     << "\tPhi1\t" << cons->GetPhi1() << "\tPhi2\t" << cons->GetPhi2()  
	     << "\tRmin1\t" << cons->GetRmin1() << "\tRmax1\t" << cons->GetRmax1()  
	     << "\tRmin2\t" << cons->GetRmin2() << "\tRmax2\t" << cons->GetRmax2() 
	     << endl;
	break;
      case TGeoShape::kGeoArb8:    
      case TGeoShape::kGeoTrap:    
	arb8 = (TGeoArb8 *) shape;
	XY = arb8->GetVertices();
	dZ = arb8->GetDz();
	for (j = 0; j < 2; j++) {
	  cout << "Trap/Arb8\tdZ\t";
	  if (j == 0) cout << -dZ;
	  else        cout <<  dZ;
	  for (i = 4*j; i < 4*j+4; i++) 
	    cout << "\t(" << XY[2*i] << "," << XY[2*i+1] << ")";
	  cout << endl;
	}
	break;
      case TGeoShape::kGeoEltu:    
	eltu = (TGeoEltu *) shape;
	cout << "Eltu\tdZ\t" << eltu->GetDz() 
	     << "\tA\t" << eltu->GetA() << "\tB\t" << eltu->GetB()  
	     << endl;
	break;
      case TGeoShape::kGeoTorus:
      case TGeoShape::kGeoPara:    
      case TGeoShape::kGeoSph:     
      case TGeoShape::kGeoCtub:    
      default:
	cout << bit << "\t has not yet implemented for " << shape->GetName() << endl;
	break;
      }
      break;
    }
  }
}
//________________________________________________________________________________
Double_t StiVMCToolKit::GetShapeVolume(TGeoShape *shape) {
  TGeoBBox *box = 0, *Box = 0; 
  TGeoTrd1 *trd1 = 0;
  TGeoTrd2 *trd2 = 0;
  TGeoTube *tube = 0;
  TGeoTubeSeg *tubs = 0;
  TGeoPcon *pcon = 0;
  TGeoPgon *pgon = 0;
  TGeoCone *cone = 0;
  TGeoConeSeg *cons = 0;
  TGeoEltu *eltu = 0;
  Double_t volume = 0;
  Double_t paramsBC[4];
  Double_t volBB = 0;
  Double_t volBC = 0;
  Int_t i;
  Int_t Nz = 0;
  shape->GetBoundingCylinder(paramsBC);
  shape->ComputeBBox();
  Box = (TGeoBBox *) shape;
  volBB = 8*Box->GetDX()*Box->GetDY()*Box->GetDZ();
  volBC = 2*TMath::Pi()*(paramsBC[1] - paramsBC[0])*Box->GetDZ();;
  for (Int_t bit = 24; bit >= 9; bit--) {//cout << bit << "\t"; 
    if (shape->TestShapeBit(BIT(bit))) {
      Int_t kbit = BIT(bit);
      switch (kbit) {
      case TGeoShape::kGeoBox: 
	box = (TGeoBBox *) shape;  
	volume = 8*box->GetDX()*box->GetDY()*box->GetDZ();
	break;
      case TGeoShape::kGeoTrd1:
	trd1 = (TGeoTrd1 *) shape; 
	volume = 4*(trd1->GetDx1()+trd1->GetDx2())*trd1->GetDy()*trd1->GetDz();
	break;   
      case TGeoShape::kGeoTrd2:
	trd2 = (TGeoTrd2 *) shape; 
	volume = 2*(trd2->GetDx1() + trd2->GetDx2())*(trd2->GetDy1() + trd2->GetDy2())*trd2->GetDz();
	break;   
      case TGeoShape::kGeoTubeSeg:  
      case TGeoShape::kGeoTube:
	tube = (TGeoTube *) shape; 
	volume = 2*TMath::Pi()*(tube->GetRmax()* tube->GetRmax() - tube->GetRmin()*tube->GetRmin())*
	  tube->GetDz();
	if (kbit == TGeoShape::kGeoTubeSeg) {
	  tubs = (TGeoTubeSeg *) shape; 
	  volume *= TMath::Abs(tubs->GetPhi2()-tubs->GetPhi1())/360.;
	}
	break;
      case TGeoShape::kGeoPcon:    
      case TGeoShape::kGeoPgon:    
	pcon = (TGeoPcon *) shape;
	Nz = pcon->GetNz();
	volume = 0;
	for (i = 1; i < Nz; i++) {
	  volume +=
	    ((pcon->GetRmax(i)*pcon->GetRmax(i) + pcon->GetRmax(i-1)*pcon->GetRmax(i-1) + pcon->GetRmax(i)*pcon->GetRmax(i-1)) -
	     (pcon->GetRmin(i)*pcon->GetRmin(i) + pcon->GetRmin(i-1)*pcon->GetRmin(i-1) + pcon->GetRmin(i)*pcon->GetRmin(i-1)))*
	    TMath::Abs((pcon->GetZ(i) - pcon->GetZ(i-1)));
	}
	if (kbit == TGeoShape::kGeoPgon) {
	  pgon = (TGeoPgon *) shape;
	  volume *= TMath::Tan(TMath::Pi()/180.*TMath::Abs(pgon->GetDphi()/pgon->GetNedges()/2.))/3.*pgon->GetNedges();
	}
	else {
	  volume *= TMath::Pi()/3.*TMath::Abs(pcon->GetDphi())/360.;
	}
	break;
      case TGeoShape::kGeoCone:    
      case TGeoShape::kGeoConeSeg: 
	cone = (TGeoCone *) shape;
	volume = TMath::Pi()*2./3.*
	  ((cone->GetRmax2()*cone->GetRmax2() + cone->GetRmax1()*cone->GetRmax1() + cone->GetRmax2()*cone->GetRmax1()) -
	   (cone->GetRmin2()*cone->GetRmin2() + cone->GetRmin1()*cone->GetRmin1() + cone->GetRmin2()*cone->GetRmin1()))*
	  cone->GetDz();
	if (kbit == TGeoShape::kGeoConeSeg) {
	  cons = (TGeoConeSeg *) shape;
	  volume *= TMath::Abs(cons->GetPhi2() - cons->GetPhi1())/360.;
	}
	break;
      case TGeoShape::kGeoEltu:    
	eltu = (TGeoEltu *) shape;
	volume = 2*TMath::Pi()*eltu->GetA()*eltu->GetB()*eltu->GetDz();
	break;
      case TGeoShape::kGeoArb8:    
      case TGeoShape::kGeoTrap:    
      case TGeoShape::kGeoTorus:
      case TGeoShape::kGeoPara:    
      case TGeoShape::kGeoSph:     
      case TGeoShape::kGeoCtub:    
      default:
	if (Debug()) 
	  cout << bit << "\t has not yet implemented for " << shape->GetName() << endl;
	volume = TMath::Min(volBB,volBC);
	break;
      }
      break;
    }
  }
  if (Debug())
    cout << " =============> Volume = " << volume 
	 << "\t" << shape->GetName() << "\tBB\t" << volBB << "\tBC\t" << volBC << endl;
  assert(volume >= 0);
  if (Debug() && ! (volBB - volume > -1e-7 && volBC - volume > -1.e-7) ) {
    PrintShape(shape);
    cout << "volume\t" << volume << "\tvolBB\t" <<  volBB << "\tvolBC\t" << volBC << endl;
    cout << "\tBoundingBox dX\t" << Box->GetDX() << "\tdY\t" << Box->GetDY() << "\tdZ\t" << Box->GetDZ() << endl;
    cout  << "\tBoundingCylinder\trMin\t" << TMath::Sqrt(paramsBC[0]) << "\trMax\t" << TMath::Sqrt(paramsBC[1])  
	  << "\tdZ\t" << Box->GetDZ() << endl;
   
    assert(volBB - volume > -1e-7 && volBC - volume > -1.e-7);
  }
  return volume;
  }
//________________________________________________________________________________ 
Int_t StiVMCToolKit::Add2ElementList(Int_t NElem,const TGeoMaterial *mat, Elem_t *ElementList) {
  assert(NElem>=0 && NElem<NoElemMax);
  if (! NAir) {
    NAir = sizeof(Air)/sizeof(ElemV_t);
    Double_t W = 0;
    for (Int_t i = 0; i < NAir; i++) W += Air[i].A*Air[i].V;
    for (Int_t i = 0; i < NAir; i++) Air[i].W = Air[i].A*Air[i].V/W;
  }
  if (mat->InheritsFrom("TGeoMixture")) {
    TGeoMixture *mix = (TGeoMixture *) mat;
    Int_t N = mix->GetNelements();
    Double_t *A = mix->GetAmixt();
    Double_t *Z = mix->GetZmixt();
    Double_t *W = mix->GetWmixt();
    for (Int_t i = 0; i < N; i++) {
      if (TMath::Abs(Z[i] -  7.30) < 1.e-3 &&
	  TMath::Abs(A[i] - 14.61) < 1.e-3) {
	for (Int_t j = 0; j < NAir; j++) {
	  ElementList[NElem].index = NElem+1;
	  ElementList[NElem].W = W[i]*Air[j].W;
	  ElementList[NElem].A = Air[j].A;
	  ElementList[NElem].Z = Air[j].Z;
	  NElem++;
	}
      } else {
	if (TMath::Abs(Z[i] - 17.436 ) < 1.e-3 &&
	    TMath::Abs(A[i] - 38.691)  < 1.e-3) {
	  for (Int_t j = 0; j < NP10; j++) {
	    ElementList[NElem].index = NElem+1;
	    ElementList[NElem].W = W[i]*P10[j].W;
	    ElementList[NElem].A = P10[j].A;
	    ElementList[NElem].Z = P10[j].Z;
	    NElem++;
	  }
	} else {
	  ElementList[NElem].index = NElem+1;
	  ElementList[NElem].W = W[i];
	  ElementList[NElem].A = A[i];
	  ElementList[NElem].Z = Z[i];
	  NElem++;
	}
      }
    }
  } else  {
    Double_t A = mat->GetA();
    Double_t Z = mat->GetZ();
    if (TMath::Abs(Z -  7.30) < 1.e-3 &&
	TMath::Abs(A - 14.61) < 1.e-3) {
      for (Int_t j = 0; j < NAir; j++) {
	ElementList[NElem].index = NElem+1;
	ElementList[NElem].W = Air[j].W;
	ElementList[NElem].A = Air[j].A;
	ElementList[NElem].Z = Air[j].Z;
	NElem++;
      }
    } else {
      if (TMath::Abs(Z - 17.436 ) < 1.e-3 &&
	  TMath::Abs(A - 38.691)  < 1.e-3) {
	for (Int_t j = 0; j < NP10; j++) {
	  ElementList[NElem].index = NElem+1;
	  ElementList[NElem].W = P10[j].W;
	  ElementList[NElem].A = P10[j].A;
	  ElementList[NElem].Z = P10[j].Z;
	  NElem++;
	}
      } else {
	ElementList[NElem].index = NElem+1;
	ElementList[NElem].W = 1.;
	ElementList[NElem].A = A;
	ElementList[NElem].Z = Z;
	NElem++;
      }
    }
  }
  assert(NElem < NoElemMax);
  return NElem;
}
//________________________________________________________________________________ 
Int_t StiVMCToolKit::Merge2ElementList(Int_t NElem, Elem_t *ElementList, 
						Int_t NElemD, Elem_t *ElementListD, Double_t weight) {
  assert(NElem>=0 && NElem<NoElemMax);
      for (Int_t i = 0; i < NElemD; i++) {
	ElementList[NElem].index = NElem+1;
	ElementList[NElem].W = weight*ElementListD[i].W;
	ElementList[NElem].A = ElementListD[i].A;
	ElementList[NElem].Z = ElementListD[i].Z;
	NElem++;
	assert(NElem < NoElemMax);
      }
  return NElem;
}
//________________________________________________________________________________
Int_t StiVMCToolKit::NormolizeElementList(Int_t NElem, Elem_t *ElementList){
  assert(NElem>=0 && NElem<NoElemMax);
  Double_t W = 0;
  for (Int_t i = 0; i < NElem; i++) W += ElementList[i].W ;
  for (Int_t i = 0; i < NElem; i++) ElementList[i].W /= W;
  for (Int_t k = 0; k < NElem; k++) {
    if (ElementList[k].W > 0) {
      for (Int_t l = k+1; l < NElem; l++) {
	if (ElementList[l].W > 0                 &&
	    ElementList[k].A == ElementList[l].A &&
	    ElementList[k].Z == ElementList[l].Z) {
	  ElementList[k].W += ElementList[l].W;
	  ElementList[l].W = -1;
	}
      }
    }
  }
  Int_t N = 0;
  
  for (Int_t k = 0; k < NElem; k++) {
    if (ElementList[k].W <= 0) 	continue;
    if (k != N)
      ElementList[N] = ElementList[k];
    N++;
  }
  return N;
}
//________________________________________________________________________________ 
Double_t StiVMCToolKit::GetWeight(TGeoNode *nodeT, const TString &pathT, 
					     Int_t *NElem, Elem_t *ElementList) {
  Double_t Weight  = 0;
  Double_t WeightT = 0;
  if (! nodeT) {
    if (! gGeoManager) return Weight;
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT.Data());
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return Weight;
  }
  TGeoVolume *volT = nodeT->GetVolume();
  const TGeoMaterial *mat = volT->GetMaterial();

  Double_t dens = mat->GetDensity();
  TGeoShape *shapeT = (TGeoShape *) volT->GetShape();
  if (! shapeT) return Weight;
  Double_t volume = GetShapeVolume(shapeT);
  WeightT = Weight = dens*volume;
#if 0  
  if (! ElementList ) {
    ElementList = new Elem_t[NoElemMax];
    NElem = new Int_t[1]; NElem[0] = 0;
  }
#else
  assert(ElementList && NElem);
#endif  
  Int_t im1 = NElem[0];
  NElem[0] = __addelement__(NElem[0], mat, ElementList);
  Int_t im2 = NElem[0];
  //  Double_t x0 = mat->GetRadLen();
  //  cout << mat->GetName() << "\tdens = " << dens << "\tx0 = " << x0 << endl;
  TObjArray *nodes = volT->GetNodes();
  Int_t nd = nodeT->GetNdaughters();
  //
  if (nd > 0) {
    vector<Double_t> weights(nd);
    for (Int_t id = 0; id < nd; id++) {
      TGeoNode *node = (TGeoNode*)nodes->UncheckedAt(id);
      if (! node) continue;
      //    if (node->IsOverlapping()) continue;
      TGeoVolume *vol = node->GetVolume();
      if (! vol) continue; 
      TString name(TString(vol->GetName())); //cout << "Ask for " << name << endl;
      //      if (name.BeginsWith("FTPC")) continue;
      TGeoShape *shapeD = (TGeoShape *) vol->GetShape();
      if (! shapeD ) continue;
      Double_t weight = dens*GetShapeVolume(shapeD);// cout << "\tWeight\t" << Weight << endl;
      WeightT -= weight;
      Weight  -= weight;
      TString path = pathT;
      if (path != "") path += "/";
      path += node->GetName();
      Int_t  NElemD[1] = {0};
      Elem_t ElementListD[NoElemMax];
      weights[id] = GetWeight(node,path,NElemD,ElementListD);
      Weight += weights[id]; // cout << "\tWeight\t" << Weight << endl;
      NElem[0] = Merge2ElementList(NElem[0], ElementList, NElemD[0], ElementListD, weights[id]);
    }
  } // nd > 0
  for (Int_t i = im1; i < im2; i++) {// update mother weight fraction
    ElementList[i].W *= WeightT;
  }
  NElem[0] = NormolizeElementList(NElem[0],ElementList);
  return Weight;
}

//________________________________________________________________________________ 
Double_t StiVMCToolKit::GetVolumeWeight(TGeoVolume *volT, Int_t *NElem, Elem_t *ElementList) 
{
  if (! volT || !volT->GetShape()) return 0;
  
  Double_t Weight = 0;
  Double_t WeightT = 0;
  const TGeoMaterial *mat = volT->GetMaterial();
  TGeoShape *shapeT = (TGeoShape *) volT->GetShape();
  if (! shapeT) return 0;

  Double_t dens = mat->GetDensity();
  if (Debug())
    cout << "volT \t" << volT->GetName() << "\t" << volT->GetTitle() << endl;
  Double_t volume = GetShapeVolume(shapeT);
  Weight = dens*volume;
  WeightT = Weight;
#if 0  
  if (! ElementList ) {
     
    ElementList = new Elem_t[NoElemMax];
    NElem = new Int_t[1]; NElem[0] = 0;
  }
#else
  assert(ElementList && "StiVMCToolKit::GetWeight: Create the the array first!");
#endif
  Int_t im1 = *NElem;
  Int_t im2 = __addelement__(im1, mat, ElementList);
  *NElem    = im2;
  //  Double_t x0 = mat->GetRadLen();
  //  cout << mat->GetName() << "\tdens = " << dens << "\tx0 = " << x0 << endl;
  TObjArray *nodes = volT->GetNodes();
  Int_t nd = volT->GetNdaughters();
  //
  if (nd > 0) {
    vector<Double_t>  weights(nd);
    for (Int_t id = 0; id < nd; id++) {
      TGeoNode *node = (TGeoNode*) nodes->UncheckedAt(id);
      if (! node) continue;
      //    if (node->IsOverlapping()) continue;
      TGeoVolume *vol = node->GetVolume();
      if (! vol) continue; 
      TString name(TString(vol->GetName())); //cout << "Ask for " << name << endl;
      //      if (name.BeginsWith("FTPC")) continue;
      TGeoShape *shapeD = (TGeoShape *) vol->GetShape();
      if (! shapeD ) continue;
      Double_t weight = dens*GetShapeVolume(shapeD);// cout << "\tWeight\t" << Weight << endl;
      WeightT -= weight;
      Weight  -= weight;
      Int_t  NElemD[1] = {0};
      Elem_t ElementListD[NoElemMax];
      weights[id] = GetVolumeWeight(vol,NElemD,ElementListD);
      Weight += weights[id]; // cout << "\tWeight\t" << Weight << endl;
      Int_t im =*NElem;
      Int_t imm = Merge2ElementList(im, ElementList, NElemD[0], ElementListD, weights[id]);
      *NElem = imm;
    }
  } // nd > 0
  for (Int_t i = im1; i < im2; i++) {// update mother weight fraction
    ElementList[i].W *= WeightT;
  }
  Int_t in = *NElem;
  Int_t inn= NormolizeElementList(in,ElementList);
  *NElem = inn;
  return Weight;
}
//________________________________________________________________________________
void StiVMCToolKit::MakeAverageVolume(TGeoVolume *volT, TGeoShape *&newshape, TGeoMedium *&newmed, Double_t *xyzM) {
  if (! volT) return;
  vector<Elem_t> ElementList(NoElemMax);
  Int_t NElem = 0;
  // NElem = ElementList.size()-1;
  Int_t *al = &NElem;
  Elem_t *el = &ElementList[0];
  TGeoVolume *v = volT;
//  Double_t Weight = GetWeight(volT, &NElem, &ElementList[0]); 
  Double_t Weight = 
                GetVolumeWeight(v
                , al
                , el
  );
  const TGeoMedium   *med = volT->GetMedium(); 
  const TGeoMaterial *mat = volT->GetMaterial();
  if (Debug()) {
    if (med->GetParam(0)) cout << "===================" << endl; 
    else                  cout << "+++++++++++++++++++" << endl;
    cout << volT->GetName() << " === "   
	 << " Weight " << Weight << "[g]\t";
    cout << "material\t" << mat->GetName() << "\tmedium\t" << med->GetName() << endl;
  }
  TGeoShape *shapeT = (TGeoShape *) volT->GetShape();
  Double_t volume = GetShapeVolume(shapeT);
  if (Debug()) PrintShape(shapeT);
  shapeT->ComputeBBox();  
  TGeoBBox * Box = (TGeoBBox *) shapeT; 
  Double_t dx=-2001, dy=-2002, dz=-2003, rmin=-2009, rmax=-2010;
  Double_t paramsBC[4];
  shapeT->GetBoundingCylinder(paramsBC);
  Double_t volBB = 8*Box->GetDX()*Box->GetDY()*Box->GetDZ();
  Double_t volBC = 2*TMath::Pi()*(paramsBC[1] - paramsBC[0])*Box->GetDZ();
  Double_t volB = 0;
  if (Debug()) cout << shapeT->GetName();
  enum ShapeId {kBox = 1, kTube, kKeep};
  Int_t iShape = 3;
  TString name(volT->GetName());
  if (name == "SROD") {// replace oval road by box
    iShape = 1;
    Double_t S = TMath::Pi()*(paramsBC[1] - paramsBC[0]);
    dy = TMath::Sqrt(paramsBC[1]) - TMath::Sqrt(paramsBC[0]);
    dx = S/(4*dy);
    dz = Box->GetDZ();
    Box->SetBoxDimensions(dx,dy,dz);
    volB = volBB = volBC;
  } else {
    if ((shapeT->TestShapeBit(TGeoShape::kGeoTube) ||
	 shapeT->TestShapeBit(TGeoShape::kGeoTubeSeg)) &&
	xyzM && xyzM[0]*xyzM[0] + xyzM[1]*xyzM[1] < 1.e-3) {
      iShape = 3;
      volB  = volume;
      if (Debug()) cout << "\tKeep shape ===========\t";
    }
    else {
      if ( (volBB <= volBC) || (xyzM && xyzM[0]*xyzM[0] + xyzM[1]*xyzM[1] > 1.e-3) ) {
	iShape = 1;
	volB = volBB;
	dx = Box->GetDX();
	dy = Box->GetDY();
	dz = Box->GetDZ();
	if (Debug()) cout << "\tBoundingBox dX\t" << dx << "\tdY\t" << dy << "\tdZ\t" << dz;
      } else {
	iShape = 2;
	volB = volBC;
	rmin = TMath::Sqrt(paramsBC[0]);
	rmax = TMath::Sqrt(paramsBC[1]);
	dz = Box->GetDZ();
	if (Debug()) cout  << "\tBoundingCylinder\trMin\t" << rmin << "\trMax\t" << rmax << "\tdZ\t" << dz;
      }
    }
  }
  Double_t Dens = Weight/volB;
  if (Debug()) cout << "\tvolume\t" << volume << "\tvolB\t" << volB << "\tDens\t" << Dens << endl;
  TString newmatname(mat->GetName());
  newmatname += "_";
  newmatname += name;
  TGeoMixture *newmat = new  TGeoMixture(newmatname.Data(),NElem, Dens);
  for (Int_t i = 0; i < NElem; i++) {
    if (Debug()) 
      cout << Form("id%3i\tW%10.3g\tA%5.1f\t%5.1f\n",
		   ElementList[i].index,ElementList[i].W,ElementList[i].A,ElementList[i].Z);
    //    cout << "id\t" << ElementList[i].index << "\tW\t" << ElementList[i].W
    //	 << "\tA\t" <<  ElementList[i].A << "\tZ\t" << ElementList[i].Z << endl;
    newmat->DefineElement(i,ElementList[i].A,ElementList[i].Z,ElementList[i].W);
  }
  newmat->SetUniqueID(gGeoManager->AddMaterial(newmat));
  Int_t matid = newmat->GetUniqueID();
  //                                  name              numed  imat, 
  Double_t params[10];
  for (Int_t i = 0; i < 10; i++) params[i] = med->GetParam(i);
  newmed = new TGeoMedium(newmatname.Data(),matid,newmat,params);
  if (Debug() && xyzM) {
    cout << "\txyzM  x\t" << xyzM[0] << "\ty\t" << xyzM[1] << "\tz\t" << xyzM[2] << endl;
  }
  newshape = shapeT;
  if (iShape == 1)  newshape = new TGeoBBox(dx, dy, dz);
  else if (iShape == 2) {
    newshape = new TGeoTube(rmin, rmax, dz);
  }
}
//________________________________________________________________________________
TGeoManager  * StiVMCToolKit::GetVMC() {
  TGeoManager *gGeo = 0;
#ifndef __NOVMC__
  /*! Load Geometry
   */
  gGeo = gGeoManager;
  if (gGeo) return gGeo;
  LOG_INFO << "StiVMCToolKit::GetVMC() : Get VMC geometry" <<endm;
  if (StMaker::GetChain()) {
    StMaker::GetChain()->GetDataBase("VmcGeometry");
  }
  if (! gGeoManager) 
    LOG_ERROR << "StiVMCToolKit::GetVMC() : Can't get VMC geometry" <<endm;
  gGeo = gGeoManager;
#endif
  return gGeo;
}
//________________________________________________________________________________
void StiVMCToolKit::TestVMC4Reconstruction(){
  /*! test with calculation volumes, weights and average densities
   */
  GetVMC();
  //  TObjArray *volumes = gGeoManager->GetListOfVolumes();
  Int_t NV[2] = {nTopVol,NofVolToBEAveraged};
  VolumeMap_t *list = 0;
  TGeoVolume *volT = 0;
  cout << "<table>" << endl;
  cout << "<tr><td>name</td><td>Comment </td><td>Volume[cm**3]</td><td>Weight[g]</td><td>Av.Dens.[g/cm**3]</td></tr>" << endl;
  for (Int_t i = 0; i < 2; i++) {
    if (i == 0) list = TopVolumes;
    else        list = VolumesToBeAveraged;
    for (Int_t j = 0; j < NV[i]; j++) {
      volT =  gGeoManager->GetVolume(list[j].name);
      //      volT = (TGeoVolume *) volumes->FindObject(list[j].name);
      if (! volT) {cout << "Can't find " << list[j].name << "\t" << list[j].comment << endl; continue;}
      TGeoShape *shapeT = volT->GetShape();
      Double_t volume = GetShapeVolume(shapeT);
      Double_t weight = GetVolumeWeight(volT, 0, 0);  // it leads to the memory leak !!!
      cout << "<tr><td>"<< list[j].name << "</td><td>" << list[j].comment << "</td>"
	   << "<td>" << Form("%10.3g",volume) << "</td>" 
	   << "<td>" << Form("%10.3g",weight) << "</td>" 
	   << "<td>" << Form("%10.3g",weight/volume) << "</td></tr>" 
	   << endl;
    }
  }
  cout << "</table>" << endl;
}
//________________________________________________________________________________
TGeoPhysicalNode *StiVMCToolKit::Alignment(const TGeoNode *nodeT, const Char_t *pathT, 
					   TGeoVolume *volT, TGeoShape *newshape, TGeoMedium* newmed) {
  // Alignment
  TObjArray *listP = gGeoManager->GetListOfPhysicalNodes();
  TGeoPhysicalNode *nodeP = 0;
  TGeoCombiTrans *trP = 0;
  if (listP) {
    Int_t N = listP->GetEntries();
    for (Int_t i = 0; i < N; i++) {
      TGeoPhysicalNode *nod = dynamic_cast<TGeoPhysicalNode *> (listP->At(i));
      if (nod && TString(nod->GetName()) == TString(pathT)) {
	nodeP = nod; break;
      }
    }
  }
  Double_t local[3] = {0,0,0};
  TGeoShape *shapeT = volT->GetShape();
  if (Debug()) {
    LOG_INFO << "StiVMCToolKit::Alignment : node\t" << nodeT->GetName() 
             << "\tvolume\t" << volT->GetName() << "\t:" << volT->GetTitle()
             << "\tshape\t" << shapeT->GetName() << "\tmaterial\t" <<  volT->GetMaterial()->GetName();
    if (newshape) LOG_INFO  << "\tnewshape\t" << newshape->GetName();
    LOG_INFO << endm;
  }
  if (shapeT->TestShapeBit(TGeoShape::kGeoPcon) || shapeT->TestShapeBit(TGeoShape::kGeoPgon)) {
    TGeoPcon *pcon = (TGeoPcon *) shapeT;
    Int_t Nz = pcon->GetNz();
    local[2] = 0.5*(pcon->GetZ(0) + pcon->GetZ(Nz-1));
  }
  Double_t master[3]; 
  nodeT->LocalToMaster(local,master);
  if (Debug()) 
    cout << "\tmaster  x\t" << master[0] << "\ty\t" << master[1] << "\tz\t" << master[2] << endl;
  if (!nodeP) nodeP = gGeoManager->MakePhysicalNode(pathT);
  if (!nodeP) return nodeP;
#if 0
  if (nodeP->IsAligned()) {
    trP = nodeP->GetNode()->GetMatrix();
    trP->SetTranslation(master[0], master[1], master[2]);
  } else {  
    trP = new TGeoCombiTrans(master[0], master[1], master[2], nodeP->GetNode()->GetMatrix());
  }  
#endif 
  nodeP->Align(trP,newshape);//,kTRUE);
  TGeoVolume *newvol = nodeP->GetNode(-1)->GetVolume();
  if (Debug()) 
    cout << "newvol\t" << newvol->GetName() << "\tmed\t" << newvol->GetMedium()->GetName() << endl;
  newvol->SetMedium(newmed);
  newvol->SetTitle("AVERAGED");
  newvol->SetLineColor(1);
  newvol->SetVisibility(1);
  TObjArray *nodes = newvol->GetNodes();
  if (nodes && ! newvol->TObject::TestBit(TGeoVolume::kVolumeImportNodes)) {
    nodes->Delete(); 
    delete nodes;
    newvol->ClearNodes();
  }
  return nodeP;
}
//________________________________________________________________________________
TGeoPhysicalNode *StiVMCToolKit::LoopOverNodes(const TGeoNode *nodeT, const Char_t *pathT, 
					       const Char_t *name,void ( *callback)(TGeoPhysicalNode *)){
  TGeoVolume *volT = nodeT->GetVolume(); 
  const Char_t *nameT = volT->GetName();
  TGeoPhysicalNode *nodeP = 0;
  TGeoShape *newshape = 0;
  TGeoMedium* newmed = 0;
  if (nameT && name && ! strncmp(nameT,name,4)) {
    //    if (TString(nameT) == TString(VolumesToBeAveraged[i].name)) {
    Double_t local[3] = {0, 0, 0};
    TGeoShape *shapeT = volT->GetShape();
    if (shapeT->TestShapeBit(TGeoShape::kGeoPcon) || shapeT->TestShapeBit(TGeoShape::kGeoPgon)) {
      TGeoPcon *pcon = (TGeoPcon *) shapeT;
      Int_t Nz = pcon->GetNz();
      local[2] = 0.5*(pcon->GetZ(0) + pcon->GetZ(Nz-1));
    }
    Double_t master[3]; 
    nodeT->LocalToMaster(local,master);
    MakeAverageVolume(volT, newshape, newmed, master);
    nodeP = Alignment(nodeT,pathT, volT, newshape, newmed);
    if (nodeP) {
      if (! callback) MakeVolume(nodeP);
      else            callback(nodeP);
    }
    return nodeP;
  }
  TObjArray *nodes = volT->GetNodes();
  Int_t nd = volT->GetNdaughters();
  for (Int_t id = 0; id < nd; id++) {
    TGeoNode *node = (TGeoNode*) nodes->UncheckedAt(id);
    if (! node) continue;
    TString path = pathT;
    if (path != "") path += "/";
    path += node->GetName();
    if (! name && Debug()) {
      cout << path;
      TGeoVolume *vol = node->GetVolume(); 
      const TGeoMedium   *med = vol->GetMedium(); 
      if (med->GetParam(0)) {cout << "\t===================" << endl; continue;}
      else                  cout << endl;
    }
    LoopOverNodes(node, path, name, callback);
  }
  return nodeP;
}
//________________________________________________________________________________
void StiVMCToolKit::MakeVolume(TGeoPhysicalNode *nodeP) {
  if (Debug()) {
    LOG_INFO << "StiVMCToolKit::MakeVolume : TGeoPhysicalNode\t" << nodeP->GetName() << endm;
    TGeoVolume   *volP   = nodeP->GetVolume();
    TGeoMaterial *matP   = volP->GetMaterial(); matP->Print("");
    TGeoShape    *shapeP = nodeP->GetShape(); cout << "New Shape\t"; PrintShape(shapeP);
    TGeoHMatrix  *hmat   = nodeP->GetMatrix(); hmat->Print("");
  }
}
//________________________________________________________________________________
Double_t StiVMCToolKit::GetPotI(const TGeoMaterial *mat) {
  Double_t PotI = 0;
  if (mat) {
    Double_t s1 = 0, s2 = 0;
    if (mat->InheritsFrom("TGeoMixture")) {
      TGeoMixture *mix = (TGeoMixture *) mat;
      Int_t N = mix->GetNelements();
      assert(N);
      Double_t *A = mix->GetAmixt();
      Double_t *Z = mix->GetZmixt();
      Double_t *W = mix->GetWmixt();
      for (Int_t i = 0; i < N; i++) {
	s1 += W[i]*Z[i]/A[i];
	s2 += W[i]*Z[i]*TMath::Log(Z[i])/A[i];
      }
      PotI=16.e-9*TMath::Exp(0.9*s2/s1);
    } else  {
      Double_t Z = mat->GetZ();
      PotI=16.e-9*TMath::Power(Z,0.9);
    }
  }
  return PotI;
}
//________________________________________________________________________________
void StiVMCToolKit::GetVMC4Reconstruction(const Char_t *pathT, const Char_t *nameT){
  /*! Loop over nodes. 
    For selected volumes make average geometry base on Boxes and 
    Tubes to match with Sti Geometry set
    Put new volumes in PhysicalNode structure
  */
  GetVMC();
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  TString path("");
  if (pathT) {gGeoManager->cd(pathT); path = pathT;}
  else       {path = gGeoManager->GetCurrentNode()->GetName();}
  TGeoNode *nodeT = gGeoManager->GetCurrentNode();
  if (! nodeT) return;
#if 1
  LoopOverNodes(nodeT, path, nameT);
#else
  for (Int_t i = 0; i < NofVolToBEAveraged; i++) {
    LoopOverNodes(nodeT, path, VolumesToBeAveraged[i].name);
  }
#endif
}
#ifndef __ROOT__
//________________________________________________________________________________
void TestVMCTK() {
  /*! Test VMC Tool Kit
   */
  // SVT weight
  TString path("HALL_1/CAVE_1/SVTT_1");
  Double_t weight = 1.e-3*StiVMCToolKit::GetWeight(0,path, 0, 0);  // It leads to the memory leak
  cout << "StiVMCToolKit::TestVMCTK() -I- total weight for " 
       << path.Data() << "\t" << weight << "[kg]" << endl;
  StiVMCToolKit::TestVMC4Reconstruction();
}
#endif
//________________________________________________________________________________
Double_t StiVMCToolKit::Nice(Double_t phi) {
  while (phi <   2*TMath::Pi()) phi += 2*TMath::Pi();
  while (phi >=  2*TMath::Pi()) phi -= 2*TMath::Pi();
  return phi; 
} 
