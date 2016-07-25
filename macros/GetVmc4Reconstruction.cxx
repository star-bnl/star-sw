//#define DEBUG
#ifdef __ROOT__
#include "StiMasterDetectorBuilder.h"
#include "StMaker.h"
#endif
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TDirectory.h"
#include "TGeoCone.h"
#include "TGeoManager.h"
#include "TGeoMaterial.h"
#include "TGeoMatrix.h"
#include "TGeoMedium.h"
#include "TGeoNode.h"
#include "TGeoParaboloid.h"
#include "TGeoPara.h"
#include "TGeoArb8.h"
#include "TGeoPatternFinder.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPhysicalNode.h"
#include "TGeoPolygon.h"
#include "TGeoShape.h"
#include "TGeoSphere.h"
#include "TGeoTorus.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoTube.h"
#include "TGeoVolume.h"
#include "TGeoXtru.h"
#else
#define BIT(n)       (1 << (n))
#endif
struct VolumeMap_t {
  Char_t *name;
  Char_t *comment;
};
struct Elem_t {
  Int_t    index;
  Double_t W;
  Double_t A;
  Double_t Z;
};
static const Int_t NoElemMax = 10000;
//________________________________________________________________________________
void /*StiMasterDetectorBuilder::*/PrintShape(TGeoShape *shape) {
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
Double_t /*StiMasterDetectorBuilder::*/GetShapeVolume(TGeoShape *shape) {
  TGeoBBox *box = 0, *Box = 0; 
  TGeoTrd1 *trd1 = 0;
  TGeoTrd2 *trd2 = 0;
  TGeoTube *tube = 0;
  TGeoTubeSeg *tubs = 0;
  TGeoPcon *pcon = 0;
  TGeoPgon *pgon = 0;
  TGeoCone *cone = 0;
  TGeoConeSeg *cons = 0;
  Double_t volume = 0;
  Double_t paramsBC[3];
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
      case TGeoShape::kGeoArb8:    
      case TGeoShape::kGeoTrap:    
      case TGeoShape::kGeoTorus:
      case TGeoShape::kGeoPara:    
      case TGeoShape::kGeoSph:     
      case TGeoShape::kGeoEltu:    
      case TGeoShape::kGeoCtub:    
      default:
#ifdef DEBUG
	cout << bit << "\t has not yet implemented for " << shape->GetName() << endl;
#endif
	volume = TMath::Min(volBB,volBC);
	break;
      }
      break;
    }
  }
#ifdef DEBUG
  cout << " =============> Volume = " << volume << "\t" << shape->GetName() << "\tBB\t" << volBB << "\tBC\t" << volBC << endl;
#endif
  assert(volume >= 0);
  if (! (volBB - volume > -1e-7 && volBC - volume > -1.e-7) ) {
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
Int_t /*StiMasterDetectorBuilder::*/Add2ElementList(Int_t NElem,const TGeoMaterial *mat, Elem_t *ElementList) {
  if (mat->InheritsFrom("TGeoMixture")) {
    TGeoMixture *mix = (TGeoMixture *) mat;
    Int_t N = mix->GetNelements();
    Double_t *A = mix->GetAmixt();
    Double_t *Z = mix->GetZmixt();
    Double_t *W = mix->GetWmixt();
    for (Int_t i = 0; i < N; i++) {
      ElementList[NElem].index = NElem+1;
      ElementList[NElem].W = W[i];
      ElementList[NElem].A = A[i];
      ElementList[NElem].Z = Z[i];
      NElem++;
      assert(NElem < NoElemMax);
    }
  }
  else  {
    ElementList[NElem].index = NElem+1;
    ElementList[NElem].W = 1.;
    ElementList[NElem].A = mat->GetA();
    ElementList[NElem].Z = mat->GetZ();
    NElem++;
    assert(NElem < NoElemMax);
  }
  return NElem;
}
//________________________________________________________________________________ 
Int_t /*StiMasterDetectorBuilder::*/Add2ElementList(Int_t NElem, Elem_t *ElementList, 
						Int_t NElemD, Elem_t *ElementListD, Double_t weight) {
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
Int_t /*StiMasterDetectorBuilder::*/NormolizeElementList(Int_t NElem, Elem_t *ElementList){
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
    if (ElementList[k].W > 0) {
      if (k > N) ElementList[N] = ElementList[k];
      N++;
    }
  }
  return N;
}
//________________________________________________________________________________ 
Double_t /*StiMasterDetectorBuilder::*/GetWeight(TGeoNode *nodeT, TString pathT, 
					     Int_t *NElem, Elem_t *ElementList) {
  Double_t Weight = 0;
  Double_t WeightT = 0;
  if (! nodeT) {
#if 0
    if (TString(gDirectory->GetName()) == "Rint") 
      TFile::Open("$STAR/StarDb/VmcGeometry/Geometry.y2005x.root");
    if (! gGeoManager) gDirectory->Get("Geometry");
#endif
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
  if (! ElementList ) {
    ElementList = new Elem_t[NoElemMax];
    memset (ElementList, 0, NoElemMax*sizeof(Elem_t));
    NElem = new Int_t[1]; NElem[0] = 0;
  }
  Int_t im1 = NElem[0];
  NElem[0] = Add2ElementList(NElem[0], mat, ElementList);
  Int_t im2 = NElem[0];
  //  Double_t x0 = mat->GetRadLen();
  //  cout << mat->GetName() << "\tdens = " << dens << "\tx0 = " << x0 << endl;
  TObjArray *nodes = volT->GetNodes();
  Int_t nd = nodeT->GetNdaughters();
  //
  if (nd > 0) {
    Double_t *weights = new Double_t[nd];
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
      Elem_t ElementListD[NoElemMax]; memset(ElementListD, 0, NoElemMax*sizeof(Elem_t));
      weights[id] = GetWeight(node,path,NElemD,ElementListD);
      Weight += weights[id]; // cout << "\tWeight\t" << Weight << endl;
      NElem[0] = Add2ElementList(NElem[0], ElementList, NElemD[0], ElementListD, weights[id]);
    }
    delete [] weights;
  } // nd > 0
  for (Int_t i = im1; i < im2; i++) {// update mother weight fraction
    ElementList[i].W *= WeightT;
  }
  NElem[0] = NormolizeElementList(NElem[0],ElementList);
  return Weight;
}
//________________________________________________________________________________ 
Double_t /*StiMasterDetectorBuilder::*/GetWeight(TGeoVolume *volT, Int_t *NElem, Elem_t *ElementList) {
  Double_t Weight = 0;
  Double_t WeightT = 0;
  if (! volT) return Weight;
  const TGeoMaterial *mat = volT->GetMaterial();

  Double_t dens = mat->GetDensity();
  TGeoShape *shapeT = (TGeoShape *) volT->GetShape();
  if (! shapeT) return Weight;
#ifdef DEBUG
  cout << "volT \t" << volT->GetName() << "\t" << volT->GetTitle() << endl;
#endif
  Double_t volume = GetShapeVolume(shapeT);
  WeightT = Weight = dens*volume;
  if (! ElementList ) {
    ElementList = new Elem_t[NoElemMax];
    memset (ElementList, 0, NoElemMax*sizeof(Elem_t));
    NElem = new Int_t[1]; NElem[0] = 0;
  }
  Int_t im1 = NElem[0];
  NElem[0] = Add2ElementList(NElem[0], mat, ElementList);
  Int_t im2 = NElem[0];
  //  Double_t x0 = mat->GetRadLen();
  //  cout << mat->GetName() << "\tdens = " << dens << "\tx0 = " << x0 << endl;
  TObjArray *nodes = volT->GetNodes();
  Int_t nd = volT->GetNdaughters();
  //
  if (nd > 0) {
    Double_t *weights = new Double_t[nd];
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
      Elem_t ElementListD[NoElemMax]; memset(ElementListD, 0, NoElemMax*sizeof(Elem_t));
      weights[id] = GetWeight(vol,NElemD,ElementListD);
      Weight += weights[id]; // cout << "\tWeight\t" << Weight << endl;
      NElem[0] = Add2ElementList(NElem[0], ElementList, NElemD[0], ElementListD, weights[id]);
    }
    delete [] weights;
  } // nd > 0
  for (Int_t i = im1; i < im2; i++) {// update mother weight fraction
    ElementList[i].W *= WeightT;
  }
  NElem[0] = NormolizeElementList(NElem[0],ElementList);
  return Weight;
}
//________________________________________________________________________________
void /*StiMasterDetectorBuilder::*/MakeAverageVolume(TGeoVolume *volT) {
  if (! volT) return;
  Elem_t *ElementList = new Elem_t[NoElemMax];
  memset (ElementList, 0, NoElemMax*sizeof(Elem_t));  
  Int_t NElem = 0;
  Double_t Weight = 1.e-3*GetWeight(volT, &NElem, ElementList); 
  cout << volT->GetName() << "\t" << volT->GetTitle() << "\t" << Form("%10.3f",Weight) << endl;
  const TGeoMedium   *med = volT->GetMedium(); 
  const TGeoMaterial *mat = volT->GetMaterial();
  if (med->GetParam(0)) cout << "===================" << endl; 
  else                  cout << "+++++++++++++++++++" << endl;
  cout << volT->GetName() << " === "   
       << " Weight " << Weight << "[g]\t";
  cout << "material\t" << mat->GetName() << "\tmedium\t" << med->GetName() << endl;
  TGeoShape *shapeT = (TGeoShape *) volT->GetShape();
  PrintShape(shapeT);
  shapeT->ComputeBBox();  
  TGeoBBox * Box = (TGeoBBox *) shapeT; 
  Double_t dx, dy, dz, rmin, rmax;
  Double_t paramsBC[3];
  shapeT->GetBoundingCylinder(paramsBC);
  Double_t volBB = 8*Box->GetDX()*Box->GetDY()*Box->GetDZ();
  Double_t volBC = 2*TMath::Pi()*(paramsBC[1] - paramsBC[0])*Box->GetDZ();
  Double_t volB = 0;
  cout << shapeT->GetName();
  Int_t ibox = 1;
  TString name(volT->GetName());
  if (name == "SROD") {// replace oval road by box
    Double_t S = TMath::Pi()*(paramsBC[1] - paramsBC[0]);
    Double_t dy = TMath::Sqrt(paramsBC[1]) - TMath::Sqrt(paramsBC[0]);
    Double_t dx = S/(4*dy);
    Double_t dz = Box->GetDZ();
    Box->SetBoxDimensions(dx,dy,dz);
    volBB = volBC;
  }
  if (volBB <= volBC) {
    volB = volBB;
    dx = Box->GetDX();
    dy = Box->GetDY();
    dz = Box->GetDZ();
    cout << "\tBoundingBox dX\t" << dx << "\tdY\t" << dy << "\tdZ\t" << dz;
  } else {
    ibox = 0;
    volB = volBC;
    rmin = TMath::Sqrt(paramsBC[0]);
    rmax = TMath::Sqrt(paramsBC[1]);
    dz = Box->GetDZ();
    cout  << "\tBoundingCylinder\trMin\t" << rmin << "\trMax\t" << rmax << "\tdZ\t" << dz;
  }
  Double_t Dens = Weight/volB;
  Double_t volume = GetShapeVolume(shapeT);
  cout << "\tvolume\t" << volume << "\tvolB\t" << volB << "\tDens\t" << Dens << endl;
#if 0
  Double_t local[3] = {0,0,0};
  if (shapeT->TestShapeBit(TGeoShape::kGeoPcon) || shapeT->TestShapeBit(TGeoShape::kGeoPgon)) {
    TGeoPcon *pcon = (TGeoPcon *) shapeT;
    Int_t Nz = pcon->GetNz();
    local[2] = 0.5*(pcon->GetZ(0) + pcon->GetZ(Nz-1));
  }
  Double_t master[3]; 
  Double_t top[3];
  nodeT->LocalToMaster(local,master);
  gGeoManager->MasterToTop(master,top);
  cout << "\tmaster  x\t" << master[0] << "\ty\t" << master[1] << "\tz\t" << master[2] << endl;
  cout << "\ttop \tx\t" << top[0] << "\ty\t" << top[1] << "\tz\t" << top[2] << endl;
#endif
  TString newmatname(mat->GetName());
  newmatname += "_";
  newmatname += name;
  TGeoMixture *newmat = new  TGeoMixture(newmatname.Data(),NElem, Dens);
  for (Int_t i = 0; i < NElem; i++) {
    cout << "id\t" << ElementList[i].index << "\tW\t" << ElementList[i].W
	 << "\tA\t" <<  ElementList[i].A << "\tZ\t" << ElementList[i].Z << endl;
    newmat->DefineElement(i,ElementList[i].A,ElementList[i].Z,ElementList[i].W);
  }
  newmat->SetUniqueID(gGeoManager->AddMaterial(newmat));
  Int_t matid = newmat->GetUniqueID();
  //                                  name              numed  imat, 
  TGeoMedium *newmed = new TGeoMedium(newmatname.Data(),matid,matid,
				      //isvol, ifield, fieldm,  tmaxfd, stemax,   deemax, epsil,   stmin                 
				      0      ,      0,     20,      20,     10,0.2488534,  1e-2,1.150551);
  TGeoShape *shapeP = 0;
  if (ibox)  shapeP = new TGeoBBox(dx, dy, dz);
  else       shapeP = new TGeoTube(rmin, rmax, dz);
#if 0
  // Alignment
  TObjArray *listP = gGeoManager->GetListOfPhysicalNodes();
  TGeoPhysicalNode *nodeP = 0;
  TGeoTranslation *trP = 0;
  if (listP) {
    Int_t N = listP->GetEntries();
    for (Int_t i = 0; i < N; i++) {
      TGeoPhysicalNode *nod = (TGeoPhysicalNode *) listP->At(i);
      if (TString(nod->GetName()) == TString(nodeT->GetName())) {
	nodeP = nod; break;
      }
    }
  }
  if (!nodeP) nodeP = gGeoManager->MakePhysicalNode(pathT.Data());
  if (nodeP->IsAligned()) {
    trP = (TGeoTranslation*)nodeP->GetNode()->GetMatrix();
    trP->SetTranslation(master[0], master[1], master[2]);
  } else {  
    trP = new TGeoTranslation(master[0], master[1], master[2]);
  }   
  nodeP->Align(trP,shapeP,kTRUE);
  TGeoVolume *newvol = nodeP->GetNode(-1)->GetVolume();
  cout << "\tnewvol" << newvol->GetName() << "\tmed\t" << newvol->GetMedium()->GetName() << endl;
  newvol->SetMedium(newmed);
  newvol->SetLineColor(1);
  newvol->SetVisibility(1);
  cout << "\tnewvol" << newvol->GetName() << "\tmed\t" << newvol->GetMedium()->GetName() << endl;`g478
  TObjArray *nodes = newvol->GetNodes();
  if (nodes && ! newvol->TObject::TestBit(TGeoVolume::kVolumeImportNodes)) {
    nodes->Delete(); 
    delete nodes;
    newvol->ClearNodes();
  }
  cout << nodeP->GetNode()->GetVolume()->GetName();
  cout << "\tshape\t" << shapeT->GetName() << "\t" << shapeP->GetName() << "\t" << volT->GetShape()->GetName() << endl;
#endif
  delete [] ElementList;
}
//________________________________________________________________________________
void /*StiMasterDetectorBuilder::*/GetVmc4Reconstruction() {
  VolumeMap_t SVTD[] = { 
    //  {"SFMO", "the mother of all Silicon Strip Detector volumes"},
    {"SCMP","SSD mounting plate inserted in the cone"},
    {"SCVM","SSD V-shape mouting piece"},
    {"SSLT","the linking (sector to the cone) tube"},
    {"SSLB","the linking (sector to the cone)"},
    {"SSRS","the side of the small rib"},
    {"SSRT","the top of the side rib"},
    {"SSSS","Side parts of the small sectors"},
    {"SSST","Top parts of the small sectors"},
    {"SFLM","the mother of the ladder"},
    {"SOUM", "Outer shileding structure"},
    {"SXRL", "Circular water feeds"},
    {"SXR1", "Circular water feeds"},
    {"SXR2", "Circular water feeds"},
    {"SCBM", "Mother of All Cables"},
    {"SCBL", "The bundles of cables connecting PCBs with the transition boards"},
    {"SCB1", "The bundles of cables connecting PCBs with the transition boards"},
    {"SCB2", "The bundles of cables connecting PCBs with the transition boards"},
    {"SCB3", "The bundles of cables connecting PCBs with the transition boards"},
    {"SFED", "bundles of water pipes"},
    {"SFE1", "bundles of water pipes"},
    {"SFE2", "bundles of water pipes"},
    {"SPLS", "plastic of the water pipes"},
    {"SPL1", "plastic of the water pipes"},
    {"SPL2", "plastic of the water pipes"},
    {"SALM", "aluminum shield mesh"},
    {"SOSH", "SVT outer shield"},
    {"SISH", "SVT inner shield"},
    {"SLYD", "layer mother"},
    {"SLY1", "layer mother"},
    {"SLY2", "layer mother"},
    {"SLY3", "layer mother"},
    {"SLY4", "layer mother"},
    {"SLY5", "layer mother"},
    {"SROD", "Support rod"},
    {"SBSP", "Beampipe support mother"},
    //  {"SCON", "Support cone mother"},
    {"SBWC", "water manifold to support cone bracket mother"},
    {"SWMM", "water manifold mother"},
    {"SIES", "Volume to hold inner endring screws"},
    {"SOES", "Volume to hold outer endring screws"},
    {"SBRG", "Bracket joining the end rungs"},
    {"SOER", "outer end ring"},
    {"SIRT", "inner end ring tube piece "},
    {"SIRP", "inner end ring polygon piece "},
    {"STAC", "twinax cable approximation, copper"}
  };
  Int_t nSvtVol = sizeof(SVTD)/sizeof(VolumeMap_t);
  VolumeMap_t TopVolumes[] = {
    {"BBCM","Beam Beam Counter Modules Geometry"},
    {"BTOF","the whole CTF system envelope"},
    {"CALB","the geometry of the Barrel EM Calorimeter"},
    {"ECAL","the EM EndCap Calorimeter GEOmetry"},
    {"FBOX","one Pb-Glass fpd detector"},
    {"FBO1","an other Pb-Glass fpd detector"},
    {"FTMO","the mother of the single FTPC RO barrel"},
    {"IBEM","the IBeam structure beneath the Bell reducer cone"},
    {"MAGP","the geometry of the STAR magnet"},
    {"PHMD","the Photon Multiplicity Detector"},
    {"PIPE","the STAR beam pipe mother volume"},
    {"SUPO","the geometry of the Forward TPC supports in STAR"},
    {"SVTT","the SVT geometry for STAR"},
    {"SFMO","the mother of all Silicon Strip Detector volumes (inside of SVTT)"},
    {"FTPC","the geometry of the Forward TPC in STAR (inside of SVTT)"},
    {"TPCE","the TPC system in STAR"},
    {"UPST","the geometry  of the UPSTREAM AreA."},
    {"VPDD","the Pseudo Vertex Position Detector of STAR"},
    {"ZCAL","the geometry of the Zero deg. Quartz Calorimeter"}
  };
  Int_t nTopVol = sizeof(TopVolumes)/sizeof(VolumeMap_t);
  if (0) cout << nTopVol << nSvtVol << endl;
#ifdef __ROOT__
  if (! gGeoManager) {
    cout << "/*StiMasterDetectorBuilder::*/GetVmc4Reconstration() -I- Get VMC geometry" <<endl;
    if (! StMaker::GetChain()) {
      cout << "/*StiMasterDetectorBuilder::*/GetVmc4Reconstration() -I- There is no chain. Get geometry for y2005x" <<endl;
      TFile::Open("$STAR/StarDb/VmcGeometry/Geometry.y2005x.root");
    } else {
      StMaker::GetChain()->GetDataBase("VmcGeometry");
    }
  }
#else
    if (TString(gDirectory->GetName()) == "Rint") 
      TFile::Open("$STAR/StarDb/VmcGeometry/Geometry.y2005x.root");
    if (! gGeoManager) gDirectory->Get("Geometry");
#endif
  if (! gGeoManager) {
    cout << "/*StiMasterDetectorBuilder::*/GetVmc4Reconstration() -E- Can't get VMC geometry" <<endl;
    return;
  }
#if 0
  // SVT
  TString path("HALL_1/CAVE_1/SVTT_1");
  Double_t weight = 1.e-3*GetWeight(0,path.Data(), 0, 0);
  cout << "/*StiMasterDetectorBuilder::*/GetVmc4Reconstration() -I- total weight for " 
       << path.Data() << "\t" << weight << "[kg]" << endl;
#endif
  TObjArray *volumes = gGeoManager->GetListOfVolumes();
  Int_t NV[2] = {nTopVol,nSvtVol};
  VolumeMap_t *list = 0;
  TGeoVolume *volT = 0;
  //#ifdef DEBUG
  cout << "<table>" << endl;
  cout << "<tr><td>name</td><td>Comment                                           </td><td>Weight[kg]</td></tr>" << endl;
  for (Int_t i = 0; i < 2; i++) {
    if (i == 0) list = TopVolumes;
    else        list = SVTD;
    for (Int_t j = 0; j < NV[i]; j++) {
      volT = (TGeoVolume *) volumes->FindObject(list[j].name);
      if (! volT) {cout << "Can't find " << list[j].name << "\t" << list[j].comment << endl; continue;}
      Double_t weight = 1.e-3*GetWeight(volT, 0, 0);
      cout << "<tr><td>"<< list[j].name << "</td><td>" << list[j].comment << "</td><td>" 
	   << Form("%10.3f",weight) << "</td></tr>" << endl;
    }
  }
  cout << "</table>" << endl;
  //#endif
#if 0 
  Elem_t *ElementList = new Elem_t[NoElemMax]; 
  memset (ElementList, 0, NoElemMax*sizeof(Elem_t));
  Int_t *NElem = new Int_t[1]; NElem[0] = 0;
  list = SVTD;
  for (Int_t j = 0; j < nSvtVol; j++) {
    volT = (TGeoVolume *) volumes->FindObject(list[j].name);
    if (! volT) {cout << "Can't find " << list[j].name << "\t" << list[j].comment << endl; continue;}
    MakeAverageVolume(volT);
  }
#endif 
}
