//#define __TCFIT__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TROOT.h"
#include "TString.h"
#include "TGeoManager.h"
#include "TClassTable.h"
#include "TGeoShape.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"
#include "TGeoMatrix.h"
#include "TGeoCone.h"
//#include "TGeoParaboloid.h"
#include "TGeoPara.h"
#include "TGeoArb8.h"
#include "TGeoPatternFinder.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPolygon.h"
#include "TGeoSphere.h"   
#include "TGeoTorus.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoTube.h"
#include "TGeoXtru.h"
#include "TDataSet.h"
#endif
TDataSet *CreateTable();
//________________________________________________________________________________
void Weight(const Char_t *path="HALL_1/CAVE_1/SVTT_1", const Char_t *tag = "y2007g") {
  if (gClassTable->GetID("TGeoManager") < 0) {
    gSystem->Load("libGeom");
  }
  if (! gGeoManager) {
    gROOT->LoadMacro(Form("/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/VmcGeometry/Geometry.%s.C",tag));
    CreateTable();
  }
  if (! gGeoManager) return;
  if (gGeoManager->cd(path)) {//cout << gGeoManager->GetCurrentNode()->GetName() << endl;
    TGeoVolume *vol =  gGeoManager->GetCurrentNode()->GetVolume(); //cout << vol->GetName() << endl;
    Double_t weight = vol->Weight();
#if 0
    if (TString(vol->GetName()) == "SVTT") {
      gGeoManager->RestoreMasterVolume(); cout << gGeoManager->GetCurrentNode()->GetName() << endl;
      gGeoManager->CdTop(); cout << gGeoManager->GetCurrentNode()->GetName() << endl;
      TString pathT(path);
      pathT += "/FTPC_1";
      gGeoManager->cd(pathT.Data());cout << gGeoManager->GetCurrentNode()->GetName() << endl;
      vol =  gGeoManager->GetCurrentNode()->GetVolume();  cout << vol->GetName() << endl;
      weight -= 2*vol->Weight();
      cout << path << "\tWeight = " << weight << endl;
    }
#else
    cout << path << "\tWeight = " << weight << "[kg]" << endl;
#endif
  }
}
//________________________________________________________________________________
void SvtVolumes() {
  struct Name_t {
    Char_t *name;  
    Char_t *title; 
    Int_t depth; 
    Int_t nv;
  };
  const Char_t *Top = "/HALL_1/CAVE_1";
  const Int_t N = 41;
  Name_t SvtVolNames[N] = {
    {"HALL_1/CAVE_1/SVTT_1","SVT mother volume",0,1},
    {"HALL_1/CAVE_1/SVTT_1/SFMO_1","the mother of all Silicon Strip Detector volumes",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SOUM_1","Outer shileding structure",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SXRL_%i","Circular water feeds",1,3},
    {"HALL_1/CAVE_1/SVTT_1/SCBM_1","Mother of All Cables",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SALM_1","aluminum shield mesh",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SOSH_1","SVT outer shield ",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SISH_1","SVT inner shield ",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SROD_1","Support rod",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SBSP_1","Beampipe support mother",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SCON_1","Support cone mother",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SBWC_1","water manifold to support cone bracket mother",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SWMM_1","water manifold mother",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SIES_1","Volume to hold inner endring screws",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SOES_1","Volume to hold outer endring screws",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SBRG_1","Bracket joining the end rungs",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SOER_1","outer end ring",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SIRT_1","inner end ring tube piece ",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SIRP_1","inner end ring polygon piece ",1,1},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1","electronics mother volume",3,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SECA_1","cables on electronics carriers",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SDYE_1","the ic chip on the hybrid x2",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SAGP_1","the Silver-Palladium layer",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SGLA_1","insulating glass layer",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SBOI_1","BeO substrate for hybrid",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SWCW_1","water channel water",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SWCS_1","Be side of the water channel x2",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SELE_1/SWCH_1","water channel top/bottom x2",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i","layer mother",1,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1","ladder mother",2,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1","a ladder volume",3,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/SPCB_1","the G10 PCB",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/SRHC_1","roha cell wafer supports",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STAB_1","the Berrillium tabs and the ends of the wafer carriers",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/SBER_1","the Berillium wafer carrier rails",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1","the wafer pack container",4,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1/STSI_1","a single waver container",5,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1/STSI_1/SVTD_1","an active wafer volume",6,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1/STSI_1/SVTD_1/SSIR_1","a non-sensitive up-down border of the wafer",7,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1/STSI_1/SVTD_1/SSID_1","a non-sensitive left-right border of the wafer",7,6},
    {"HALL_1/CAVE_1/SVTT_1/SLYD_%i/SLSD_1/SLDI_1/STLI_1/STSI_1/SVTD_1/STRA_1","a trapezoid of triangular shape",7,6}
  };
  TString Path;
  for (Int_t i = 0; i < N; i++) {
    for (Int_t l = 0; l < SvtVolNames[i].nv; l++) {
      if (SvtVolNames[i].nv == 1) Path = SvtVolNames[i].name;
      else                        Path = Form(SvtVolNames[i].name,l+1);
      cout << SvtVolNames[i].title << "\t";
      Weight(Path);
    }
  }
}
//________________________________________________________________________________
void PrintShape(TGeoShape *shape) {
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
  Double_t *XY;
  Double_t dZ;
  //  Double_t paramsBB[3];
  Double_t paramsBC[3];
  Int_t i, j;
  Int_t Nz = 0;
  shape->GetBoundingCylinder(paramsBC);
  shape->ComputeBBox();
  Box = (TGeoBBox *) shape;
  for (Int_t bit = 24; bit >= 9; bit--) {//cout << bit << "\t"; 
    UInt_t BITbit = 1 << bit;
    if (shape->TestShapeBit(BITbit)) {
      switch (BITbit) {
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
	tubs->GetDz()*TMath::Abs(tubs->GetPhi2()-tubs->GetPhi1())/360.;
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
      case TGeoShape::kGeoTorus:
      case TGeoShape::kGeoPara:    
      case TGeoShape::kGeoSph:     
      case TGeoShape::kGeoEltu:    
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
void PrintVolume(const Char_t *path=0) {
  if (! path) return;
  if (! gGeoManager->cd(path)) return;
  TGeoNode *nodeP = gGeoManager->GetCurrentNode();
  TGeoVolume   *volP   = nodeP->GetVolume();
  TGeoMaterial *matP   = volP->GetMaterial(); matP->Print("");
  TGeoShape    *shapeP = volP->GetShape(); cout << "Shape\t"; PrintShape(shapeP);
  //  TGeoHMatrix  *hmat   = nodeP->GetMatrix(); hmat->Print("");
} 
