// $Id: StiIstDetectorBuilder.cxx,v 1.18 2008/04/03 20:04:21 fisyak Exp $
// 
// $Log: StiIstDetectorBuilder.cxx,v $
// Revision 1.18  2008/04/03 20:04:21  fisyak
// Straighten out DB access via chairs
//
// Revision 1.17  2007/04/27 18:44:03  wleight
// Corrected a problem with incorrect assignment of hit errors
//
// Revision 1.16  2007/04/23 14:42:10  wleight
// Added new hit error calculator for outer half of 17cm layer
//
// Revision 1.14  2007/04/06 15:58:21  wleight
// Changed some cout statements to LOG_INFO
//
// Revision 1.13  2006/12/14 22:01:47  wleight
// Changed hit errors so that they are obtained from the database and are different for each layer
//
// Revision 1.12  2006/11/28 22:18:01  wleight
// Changed hit errors to 60 microns for x and 1.9 mm for y
//
// Revision 1.11  2006/10/20 18:43:12  wleight
// Changes to make perfect hits in the IST work with UPGR05
/*!
 * \class StiIstDetectorBuilder
 */

#include <stdio.h>
#include <map>
#include <exception>
using namespace std;
#include <stdexcept>
#include "StMessMgr.h"
#include "StThreeVectorD.hh"

//#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiRnD/Ist/StiIstIsActiveFunctor.h" 
#include "StiRnD/Ist/StiIstDetectorBuilder.h" 
#include "Sti/StiElossCalculator.h"
#include "StiRnD/Ist/StiIstDetectorBuilder.h"
#include "StiIst1HitErrorCalculator.h"
#include "StiIst2HitErrorCalculator.h"
#include "StiIst3HitErrorCalculator.h"
//#include "StSsdUtil/StSsdConfig.hh"
//#include "StSsdUtil/StSsdGeometry.hh"
//#include "StSsdUtil/StSsdWaferGeometry.hh"
//#include "StSsdDbMaker/StSsdDbMaker.h"
//#include "StSsdDbMaker/St_SsdDb_Reader.hh"

StiIstDetectorBuilder::StiIstDetectorBuilder(bool active, const string & inputFile)
    : StiDetectorBuilder("Ist",active,inputFile), _siMat(0), _hybridMat(0)
{
    // Hit error parameters : it is set to 20 microns, in both x and y coordinates 
}

StiIstDetectorBuilder::~StiIstDetectorBuilder()
{} 


void StiIstDetectorBuilder::buildDetectors(StMaker & source)
{
    char name[50];  
    int nRows = 1 ;
    
    setNRows(nRows);
    if (StiVMCToolKit::GetVMC()) {useVMCGeometry();}
}

//________________________________________________________________________________
void StiIstDetectorBuilder::useVMCGeometry() {
  cout << "StiIstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  struct Material_t {
    Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"SILICON", &_siMat},
    {"SILICON", &_hybridMat}
  };
  Int_t M = sizeof(map)/sizeof(Material_t);
  for (Int_t i = 0; i < M; i++) {
    const TGeoMaterial *mat =  gGeoManager->GetMaterial(map[i].name); 
    if (! mat) continue;
    Double_t PotI = StiVMCToolKit::GetPotI(mat);
    *map[i].p = add(new StiMaterial(mat->GetName(),
				    mat->GetZ(),
				    mat->GetA(),
				    mat->GetDensity(),
				    mat->GetDensity()*mat->GetRadLen(),
				    PotI));
  }
  const VolumeMap_t IstVolumes[] = { 
  // SSD
  {"IBSS", "the mother of each wafer","HALL_1/CAVE_1/IBMO_1","",""},
  };
  Int_t NoIstVols = sizeof(IstVolumes)/sizeof(VolumeMap_t);
  TString pathT("HALL_1/CAVE_1/IBMO_1");
  TString path("");
  for (Int_t i = 0; i < NoIstVols; i++) {
    gGeoManager->RestoreMasterVolume(); 
    gGeoManager->CdTop();
    gGeoManager->cd(pathT); path = pathT;
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;;
    //StiVMCToolKit::SetDebug(1);
    StiVMCToolKit::LoopOverNodes(nodeT, path, IstVolumes[i].name, MakeAverageVolume);
  }
}

void StiIstDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP) {
  LOG_DEBUG << "StiDetectorBuilder::AverageVolume -I TGeoPhysicalNode\t" << nodeP->GetName() << endm;
  // decode detector ------------------------------
  TString nameP(nodeP->GetName());
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  TString temp=nameP;
  temp.ReplaceAll("/IBMO_1/IBMY","");
  int q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num0=temp(0,1);
  int layer=num0.Atoi();
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num1=temp(0,2);
  if(!num1.IsDigit()) num1=temp(0,1);
  int ladder=num1.Atoi();
  int nWafers=10;
  if(layer==2) nWafers=13;
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num2=temp(0,2);
  if(!num2.IsDigit()) num2=temp(0,1);
  int wafer=num2.Atoi();
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num3=temp(0,1);
  int side=num3.Atoi();
  if (wafer != 1) return;
  StiDetector *pDetector = 0;
  //  if (_detectors.size() && _detectors[2*(layer-1)+side-1] getDetector(2*(layer-1)+side-1,0);
  //  if (pDetector) return;
  //----------------------------------------
  TGeoVolume   *volP   = nodeP->GetVolume();
  TGeoMaterial *matP   = volP->GetMaterial(); if (debug()) matP->Print("");
  TGeoShape    *shapeP = nodeP->GetShape();   if (debug()) {cout << "New Shape\t"; StiVMCToolKit::PrintShape(shapeP);}
  TGeoHMatrix  *hmat   = nodeP->GetMatrix();  if (debug()) hmat->Print("");
  Double_t PotI = StiVMCToolKit::GetPotI(matP);
  static StiMaterial *matS = 0;
  if (! matS) matS = add(new StiMaterial(matP->GetName(),
					 matP->GetZ(),
					 matP->GetA(),
					 matP->GetDensity(),
					 matP->GetDensity()*matP->GetRadLen(),
					 PotI));
  Double_t ionization = matS->getIonization();
  StiElossCalculator *ElossCalculator = new StiElossCalculator(matS->getZOverA(), ionization*ionization, matS->getA(), matS->getZ(),matS->getDensity());
  StiShape     *sh     = findShape(volP->GetName());
  Double_t     *xyz    = hmat->GetTranslation();
  Double_t     *rot    = hmat->GetRotationMatrix();
  Double_t      Phi    = 0;
  //  Double_t xc,yc,zc,rc,rn, nx,ny,nz,yOff;
  StiPlacement *pPlacement = 0;
  if (xyz[0]*xyz[0] + xyz[1]*xyz[1] < 1.e-3 && 
      TMath::Abs(rot[0]*rot[0] + rot[4]*rot[4] + rot[8]*rot[8] - 3) < 1e-5 &&
      (shapeP->TestShapeBit(TGeoShape::kGeoTubeSeg) ||
       shapeP->TestShapeBit(TGeoShape::kGeoTube))) {
    Double_t paramsBC[3];
    shapeP->GetBoundingCylinder(paramsBC);
    TGeoTube *shapeC = (TGeoTube *) shapeP;
    Double_t Rmax = shapeC->GetRmax();
    Double_t Rmin = shapeC->GetRmin();
    Double_t dZ   = shapeC->GetDz();
    Double_t radius = (Rmin + Rmax)/2;
    Double_t dPhi = 2*TMath::Pi();
    Double_t dR   = Rmax - Rmin;
    dR = TMath::Min(0.2*dZ, dR);
    if (dR < 0.1) dR = 0.1;
    int Nr = (int) ((Rmax - Rmin)/dR);
    if (Nr <= 0) Nr = 1;
    dR = (Rmax - Rmin)/Nr;
    if(shapeP->TestShapeBit(TGeoShape::kGeoTubeSeg)) {
      TGeoTubeSeg *shapeS = (TGeoTubeSeg *) shapeP;
      Phi =  TMath::DegToRad()*(shapeS->GetPhi2() + shapeS->GetPhi1())/2;
      Phi =  StiVMCToolKit::Nice(Phi);
      dPhi = TMath::DegToRad()*(shapeS->GetPhi2() - shapeS->GetPhi1());
    }
    for (Int_t ir = 0; ir < Nr; ir++) {
      TString Name(volP->GetName());
      if (ir > 0) {
	Name += "__";
	Name += ir;
      }
      sh     = findShape(Name.Data());
      if (! sh) {// I assume that the shape name is unique
	sh = new StiCylindricalShape(volP->GetName(), // Name
				     dZ,      // halfDepth nWafers*
				     dR,              // thickness
				     Rmin + (ir+1)*dR,// outerRadius
				     dPhi);           // openingAngle
	add(sh);
      }
      pPlacement = new StiPlacement;
      pPlacement->setZcenter(xyz[2]);
      pPlacement->setLayerRadius(Rmin + (ir+0.5)*dR);
      pPlacement->setLayerAngle(Phi);
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      pPlacement->setNormalRep(Phi,radius, 0); 
    }
  }
  else {// BBox
    shapeP->ComputeBBox();
    TGeoBBox *box = (TGeoBBox *) shapeP;
    if (! sh) {
      sh = new StiPlanarShape(volP->GetName(),// Name
			      nWafers*box->GetDZ(),   // halfDepth
			      box->GetDY(),   // thickness
			      box->GetDX());  // halfWidth
      add(sh);
    }
    // rot = {r0, r1, r2,
    //        r3, r4, r5,
    //        r6, r7, r8}
    //  double nx = rot[3];// 
    //  double ny = rot[4];
    StThreeVectorD centerVector(xyz[0],xyz[1],xyz[2]);
    StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
    Double_t phi  = centerVector.phi();
    Double_t phiD = normalVector.phi();
    Double_t r = centerVector.perp();
    pPlacement = new StiPlacement;
    //    pPlacement->setZcenter(xyz[2]);
    pPlacement->setZcenter(0);
    pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    //	  pPlacement->setNormalRep(phi, r*TMath::Cos(phi), r*TMath::Sin(phi)); 
    //    pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), -r*TMath::Sin(phi-phiD)); 
    pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
  }
  assert(pPlacement);
  pDetector = getDetectorFactory()->getInstance();
  pDetector->setName(nameP.Data());
  pDetector->setIsOn(false);
  //if(side==1)
  pDetector->setIsActive(new StiIstIsActiveFunctor);
  //else 
  //pDetector->setIsActive(new StiNeverActiveFunctor);
  pDetector->setIsContinuousMedium(false);
  pDetector->setIsDiscreteScatterer(true);
  pDetector->setShape(sh);
  pDetector->setPlacement(pPlacement); 
  pDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
  pDetector->setMaterial(matS);
  pDetector->setElossCalculator(ElossCalculator);
  if(layer==1) pDetector->setHitErrorCalculator(StiIst1HitErrorCalculator::instance());
  if(layer==2 && side==1) pDetector->setHitErrorCalculator(StiIst3HitErrorCalculator::instance());
  if(layer==2 && side==2) pDetector->setHitErrorCalculator(StiIst2HitErrorCalculator::instance());
  //  add(2*(layer-1)+side-1,wafer-1,pDetector);
  add(2*(layer-1)+side-1,ladder,pDetector);  
  //add(2*ladder-3+side,0,pDetector);
  if (debug()) {
  LOG_INFO<<"layer/ladder/wafer/side "<< layer << "/" << ladder << "/" << wafer << "/" << side << endm;
  LOG_INFO<<"the numbers defining this volume are 2*(layer-1)+side-1: "<<2*(layer-1)+side-1<<" and "<<ladder<<endm;
  }
}

