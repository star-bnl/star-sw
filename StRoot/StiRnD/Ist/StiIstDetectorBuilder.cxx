// 12/12/2012 : modification of the builder to take into account the new geometry path names
// backward compatibility with upgr15 geometry is lost

// $Id: StiIstDetectorBuilder.cxx,v 1.24 2014/08/22 17:49:18 perev Exp $
// 
// $Log: StiIstDetectorBuilder.cxx,v $
// Revision 1.24  2014/08/22 17:49:18  perev
// StiEloss calculator creates in material now
//
// Revision 1.23  2012/12/18 20:52:32  bouchet
// update for DEV13 geometry
//
// Revision 1.22  2010/10/20 20:05:06  fisyak
// Move SROD and SBSP from StiSvtDetectorBuilder to StiStarDetectorBuilder to account configurations without SVT detector installed (bug #2025)
//
// Revision 1.21  2010/08/25 21:57:41  fisyak
// Get rid off access to specfic detector tracking parameters which usage has been  disable since 2008/06/11
//
// Revision 1.20  2009/03/16 13:51:00  fisyak
// Move out all Sti Chairs into StDetectorDb
//
// Revision 1.19  2009/02/06 21:26:30  wleight
// UPGR15 Update
//
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
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiRnD/Ist/StiIstIsActiveFunctor.h" 
#include "StiRnD/Ist/StiIstDetectorBuilder.h" 
//#include "Sti/StiElossCalculator.h"
#include "StiRnD/Ist/StiIstDetectorBuilder.h"
#include "StDetectorDbMaker/StiIst1HitErrorCalculator.h"
//#include "StSsdUtil/StSsdConfig.hh"
//#include "StSsdUtil/StSsdGeometry.hh"
//#include "StSsdUtil/StSsdWaferGeometry.hh"
//#include "StSsdDbMaker/StSsdDbMaker.h"
//#include "StSsdDbMaker/St_SsdDb_Reader.hh"

StiIstDetectorBuilder::StiIstDetectorBuilder(bool active)
    : StiDetectorBuilder("Ist",active), _siMat(0), _hybridMat(0)
{
    // Hit error parameters : it is set to 20 microns, in both x and y coordinates 
    //_hitCalculator1.setName("ist1HitError");
    //_hitCalculator2.setName("ist2HitError");
    //_hitCalculator3.setName("ist3HitError");
}

StiIstDetectorBuilder::~StiIstDetectorBuilder()
{} 

void StiIstDetectorBuilder::loadDS(TDataSet& ds){
  cout<<"StiIstDetectorBuilder::loadDS(TDataSet& ds) -I- started: "<<endl;
  //_hitCalculator1.loadDS(ds);
  //_hitCalculator2.loadDS(ds);
  //_hitCalculator3.loadDS(ds);
}


void StiIstDetectorBuilder::buildDetectors(StMaker & source)
{
    int nRows = 1 ;
    gMessMgr->Info() << "StiIstDetectorBuilder::buildDetectors() - I - Started "<<endm;
    
    setNRows(nRows);
    if (StiVMCToolKit::GetVMC()) {useVMCGeometry();}
}

//________________________________________________________________________________
void StiIstDetectorBuilder::useVMCGeometry() {
  cout << "StiIstDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);

  // Build the material map
  struct Material_t {
    const Char_t *name;
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

  // Build the volume map and loop over all found volumes
  const VolumeMap_t IstVolumes[] = 
        { 
          {"IBSS", "active silicon",  "HALL_1/CAVE_1/IDSM_1/IBMO_1","",""}, 
	  {"IBSP", "inactive silicon","HALL_1/CAVE_1/IDSM_1/IBMO_1","",""}
        };
  Int_t NoIstVols = sizeof(IstVolumes)/sizeof(VolumeMap_t);
  gGeoManager->RestoreMasterVolume(); 
  gGeoManager->CdTop();
  for (Int_t i = 0; i < NoIstVols; i++) {
    gGeoManager->cd(IstVolumes[i].path); 
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;;
    StiVMCToolKit::LoopOverNodes(nodeT, IstVolumes[i].path, IstVolumes[i].name, MakeAverageVolume);
  }
}

void StiIstDetectorBuilder::AverageVolume(TGeoPhysicalNode *nodeP) {

  LOG_DEBUG << "StiDetectorBuilder::AverageVolume -I TGeoPhysicalNode\t" << nodeP->GetName() << endm;
  //cout << "StiDetectorBuilder::AverageVolume -I TGeoPhysicalNode\t" << nodeP->GetName() << endl;

  // Ugh, have to hardwire
  const Int_t NWAFERS = 12;

  TString nameP(nodeP->GetName());

  // decode detector ------------------------------
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  TString temp=nameP;
  temp.ReplaceAll("IDSM_1/IBMO_1","");
  int q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num0=temp(0,2);
  if(!num0.IsDigit()) num0=temp(0,1);
  int layer=num0.Atoi();
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num1=temp(0,2);
  if(!num1.IsDigit()) num1=temp(0,1);
  int ladder=num1.Atoi(); 
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num2=temp(0,2);
  if(!num2.IsDigit()) num2=temp(0,1);
//  int wafer=num2.Atoi();
  q=temp.Index("_");
  temp.Replace(0,q+1,"");
  TString num3=temp(0,1);
  //  int side=num3.Atoi();
  if(ladder!=1) return;

  // Check whether this is an active volume
  Bool_t ActiveVolume = kFALSE;
  if (nodeP->GetVolume()->GetMedium()->GetParam(0) == 1) {
    ActiveVolume = kTRUE;
  }

  // Material definitions
  TGeoMaterial *matP = nodeP->GetVolume()->GetMaterial();
  Double_t PotI = StiVMCToolKit::GetPotI(matP);
  static StiMaterial *matS = 0;
  if (! matS) matS = add(new StiMaterial(matP->GetName(),
					 matP->GetZ(),
					 matP->GetA(),
					 matP->GetDensity(),
					 matP->GetDensity()*matP->GetRadLen(),
					 PotI));
//  Double_t ionization = matS->getIonization();
//   StiElossCalculator *ElossCalculator = new StiElossCalculator(matS->getZOverA(), 
//                                                                ionization*ionization, 
//                                                                matS->getA(), 
//                                                                matS->getZ(),
//                                                                matS->getDensity());
  // Extract volume geometry for this node
  TGeoBBox *box = (TGeoBBox *) nodeP->GetShape();
  StiShape *sh  = new StiPlanarShape(nodeP->GetVolume()->GetName(), // Name
			             NWAFERS*box->GetDZ(),          // halfDepth
			             box->GetDY(),                  // thickness
			             box->GetDX());                 // halfWidth
  add(sh);
    
  // Position information
  TGeoHMatrix  *hmat   = nodeP->GetMatrix();
  Double_t     *xyz    = hmat->GetTranslation();
  Double_t     *rot    = hmat->GetRotationMatrix();
  StThreeVectorD centerVector(xyz[0],xyz[1],xyz[2]);
  StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
  //cout<<"centerVector: "<<centerVector<<endl;
  //cout<<"normalVector: "<<normalVector<<endl;

  // Normalize normal vector, just in case....
  normalVector /= normalVector.magnitude();

  // Volume positioning
  StiPlacement *pPlacement = new StiPlacement;
  Double_t phi  = centerVector.phi();
  Double_t phiD = normalVector.phi();
  Double_t r    = centerVector.perp();
  pPlacement->setZcenter(0);
  pPlacement->setLayerRadius(r); 
  /*
  double layerAngleOffset=(wafer-NWAFERS/2.)/25.;
  cout<<"offset: "<<layerAngleOffset<<endl;
  double layerAngle=phi+layerAngleOffset;
  cout<<"layerAngle: "<<layerAngle<<endl;
  pPlacement->setLayerAngle(layerAngle);
  */
  if(nameP.Contains("IBSS")) pPlacement->setLayerAngle(phi);
  if(nameP.Contains("IBSP")) pPlacement->setLayerAngle(phi-.05);
  pPlacement->setRegion(StiPlacement::kMidRapidity);
  pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD)); 
  assert(pPlacement);
  //cout<<"normal radius: "<<pPlacement->getNormalRadius()<<endl;
  //cout<<"normal ref angle: "<<pPlacement->getNormalRefAngle()<<endl;
   
  StiDetector *pDetector = getDetectorFactory()->getInstance();
  pDetector->setName(nameP.Data());
  pDetector->setIsOn(false);
  //addLayer is used to "create" another layer
  //if hits are from the inactive silicon (addLayer==2), they will not be used
  Int_t addLayer =0;
  if (ActiveVolume) {
    pDetector->setIsActive(new StiIstIsActiveFunctor);
    cout<<"active volume: "<<nameP<<endl;
    addLayer = 1;
  }
  else {
    pDetector->setIsActive(new StiNeverActiveFunctor);
    cout<<"inactive volume: "<<nameP<<endl;
    addLayer =2;
  }
  pDetector->setIsContinuousMedium(false);
  pDetector->setIsDiscreteScatterer(true);
  pDetector->setShape(sh);
  pDetector->setPlacement(pPlacement); 
  pDetector->setGas(GetCurrentDetectorBuilder()->getGasMat());
  pDetector->setMaterial(matS);
//  pDetector->setElossCalculator(ElossCalculator);
  pDetector->setHitErrorCalculator(StiIst1HitErrorCalculator::instance());

  // Adding detector, note that no keys are set in IST!
  //add(ladder,wafer,pDetector);
  add(addLayer,layer,pDetector);

  LOG_INFO << "StiIstDetectorBuilder: Added detector -I- " << pDetector->getName() << endm;

  // Whole bunch of debugging information
  /*
  Float_t rad2deg = 180.0/3.1415927;
  cout << "===>NEW:IST:pDetector:Name               = " << pDetector->getName()                       << endl;
  cout << "===>NEW:IST:pPlacement:NormalRefAngle    = " << pPlacement->getNormalRefAngle()*rad2deg    << endl;
  cout << "===>NEW:IST:pPlacement:NormalRadius      = " << pPlacement->getNormalRadius()              << endl;
  cout << "===>NEW:IST:pPlacement:NormalYoffset     = " << pPlacement->getNormalYoffset()             << endl;
  cout << "===>NEW:IST:pPlacement:CenterRefAngle    = " << pPlacement->getCenterRefAngle()*rad2deg    << endl;
  cout << "===>NEW:IST:pPlacement:CenterRadius      = " << pPlacement->getCenterRadius()              << endl;
  cout << "===>NEW:IST:pPlacement:CenterOrientation = " << pPlacement->getCenterOrientation()*rad2deg << endl;
  cout << "===>NEW:IST:pPlacement:LayerRadius       = " << pPlacement->getLayerRadius()               << endl;
  cout << "===>NEW:IST:pPlacement:LayerAngle        = " << pPlacement->getLayerAngle()*rad2deg        << endl;
  cout << "===>NEW:IST:pPlacement:Zcenter           = " << pPlacement->getZcenter()                   << endl;
  cout << "===>NEW:IST:pDetector:Layer             = " << layer                                       << endl;
  cout << "===>NEW:IST:pDetector:Ladder            = " << ladder                                      << endl;
  cout << "===>NEW:IST:pDetector:Wafer             = " << wafer                                       << endl;
  cout << "===>NEW:IST:pDetector:Side(defunct)     = " << side                                        << endl;
  cout << "===>NEW:IST:pDetector:Active?           = " << pDetector->isActive()                       << endl;
  */

}

