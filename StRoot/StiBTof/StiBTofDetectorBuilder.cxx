/*
 * $Id: StiBTofDetectorBuilder.cxx,v 1.1.1.1 2012/04/10 13:31:39 fisyak Exp $
 *
 * $Log: StiBTofDetectorBuilder.cxx,v $
 * Revision 1.1.1.1  2012/04/10 13:31:39  fisyak
 * The first version
 *
 * Revision 1.27  2011/04/22 22:00:18  fisyak
 * warn off
 */

#include <stdio.h>
#include <stdexcept>
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "StiBTofDetectorBuilder.h" 
#include "StDetectorDbMaker/StiBTofHitErrorCalculator.h"
#include "TDataSetIter.h"
#include "tables/St_HitError_Table.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StBTofHit.h"
/// Build all detector components of the BTof detector.
static Int_t _debug = 0;
THashList *StiBTofDetectorBuilder::fRotList = 0;
//________________________________________________________________________________
void StiBTofDetectorBuilder::buildDetectors(StMaker &source) {
  LOG_INFO << "StiBTofDetectorBuilder::buildDetectors() -I- Started" << endm;
  setNRows(1);
  setNSectors(1,StBTofHit::kNTray/2);
  fRotList = new THashList(StBTofHit::kNModule*StBTofHit::kNTray,0);
  fRotList->SetOwner(kTRUE);
  useVMCGeometry(); 
}
//________________________________________________________________________________
void StiBTofDetectorBuilder::useVMCGeometry() {
  LOG_INFO << "StiBTofDetectorBuilder::buildDetectors() -I- Use VMC geometry" 
       << endm;
  SetCurrentDetectorBuilder(this);

  // Build materials. In the BTof detector we have two: Air for the mother volume,
  // silicon for both the detector and the ladder support. This will be updated with
  // more detailed support structures at the appropriate time.
  struct Material_t {
    const Char_t *name;
    StiMaterial    **p;
  };
  Material_t map[] = {
    {"AIR", &_gasMat},
    {"BTOF_RPCG", &_fcMaterial} 
  };
  Int_t M = sizeof(map)/sizeof(Material_t);
  StiElossCalculator *ElossCalculator= 0;
  StiMaterial *matS = 0;
  for (Int_t i = 0; i < M; i++)     {
    const TGeoMaterial *mat =  gGeoManager->GetMaterial(map[i].name); 
    if (! mat) continue;
    Double_t PotI = StiVMCToolKit::GetPotI(mat);
    *map[i].p = add(new StiMaterial(mat->GetName(),
				    mat->GetZ(),
				    mat->GetA(),
				    mat->GetDensity(),
				    mat->GetDensity()*mat->GetRadLen(),
				    PotI));
    matS = *map[i].p;
    Double_t ionization = matS->getIonization();
    ElossCalculator = new StiElossCalculator(matS->getZOverA(), 
					     ionization*ionization, 
					     matS->getA(), 
					     matS->getZ(),
					     matS->getDensity());
  }
  //
  // Build volumes. Will be done from GEANT tables.
  //

  // Set volume name tree. Inactive volumes only here. Active volumes are declared in ::AverageVolume, called
  // through loop over StiDetectorBuilder::AverageVolume
  const VolumeMap_t BTofVolume =  
    {"BRSG","the sensitive gas layer in the TOFr module",   
     "HALL_1/CAVE_1/BTOF_1/BTOH_%d/BSEC_%d/BTRA_1/BXTR_1/BRTC_1/BGMT_1/BRMD_%d/BRDT_1/BRSG_3","",""}; 
  TGeoVolume *volP = gGeoManager->GetVolume(BTofVolume.name);
  if (! volP) return;  // No BTOF at all
  TGeoShape    *shapeP = volP->GetShape();  
  TGeoBBox     *box    = (TGeoBBox *) shapeP;
  StiShape     *sh     = 0;
  // Check whether this is an active volume
  Bool_t ActiveVolume = kFALSE;
  if (volP->GetMedium()->GetParam(0) == 1) {
    ActiveVolume = kTRUE;
  }
  for (Int_t sector = 1; sector <= StBTofHit::kNTray/2; sector++) {
    for (Int_t half = 1; half <= 2; half++) {
      Int_t tray = StBTofHit::kNTray/2*(half-1) + sector;
      for (Int_t module = 1; module <=  StBTofHit::kNModule; module++) {
	TString pathT(Form(BTofVolume.path,half,sector,module));
	gGeoManager->RestoreMasterVolume(); 
	gGeoManager->CdTop();
	if (! gGeoManager->CheckPath(pathT)) {
	  pathT.ReplaceAll("CAVE_1","CAVE_1/TpcRefSys_1");
	  if (! gGeoManager->CheckPath(pathT)) continue;
	}
	TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(pathT);
	//Extract volume geometry from TGeoPhysicalNode
	// position information
	TGeoHMatrix  *hmat   = nodeP->GetMatrix();  if (debug()) hmat->Print("");
	TGeoHMatrix  *newmat = new TGeoHMatrix(*hmat);
	newmat->SetName(Form("BTof_Tray_%i_Module_%i",tray,module));
	fRotList->Add(newmat);
      }
    }
    Int_t half = 1;
    Int_t tray = StBTofHit::kNTray/2*(half-1) + sector;
    Int_t module = StBTofHit::kNModule;
    TGeoHMatrix  *hmat = (TGeoHMatrix  *) fRotList->FindObject(Form("BTof_Tray_%i_Module_%i",tray,module));
    if (! hmat) continue;
    Double_t     *xyz    = hmat->GetTranslation();
    Double_t     *rot    = hmat->GetRotationMatrix();
    TString name(hmat->GetName());
    name.ReplaceAll("HALL_1/CAVE_1/BTOF_1/","");
    name.ReplaceAll("BTRA_1/BXTR_1/BRTC_1/BGMT_1/","");
    name.ReplaceAll("/BRDT_1/BRSG_3","");
    sh = new StiPlanarShape(volP->GetName(),          // Name
			    xyz[2]+box->GetDZ(),      // halfDepth of whole tray
			    box->GetDX(),             // thickness
			    StBTofHit::kNCell*StBTofHit::padWidth()/2); // box->GetDY(),         // halfWidth
    sh->setName(name.Data());
    add(sh);
    StThreeVectorD centerVector(xyz[0],xyz[1],0);
    StThreeVectorD normalVector(rot[0],rot[3],rot[6]);
    Double_t prod = centerVector*normalVector;
    if (prod < 0) normalVector *= -1;
    Double_t phi  = centerVector.phi();
    Double_t phiD = normalVector.phi();
    Double_t r = centerVector.perp();
    StiPlacement *pPlacement = new StiPlacement;
    pPlacement->setZcenter(centerVector.z());
    pPlacement->setLayerRadius(r); //this is only used for ordering in detector container...
    pPlacement->setLayerAngle(phi); //this is only used for ordering in detector container...
    pPlacement->setRegion(StiPlacement::kMidRapidity);
    pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD));
    StiDetector *det =getDetectorFactory()->getInstance();
    det->setName(name.Data());
    det->setIsOn(kTRUE);
    if (ActiveVolume) det->setIsActive(new StiIsActiveFunctor);
    else              det->setIsActive(new StiNeverActiveFunctor);
    det->setIsContinuousMedium(false);
    det->setIsDiscreteScatterer(true);
    det->setShape(sh);
    det->setPlacement(pPlacement);
    det->setGas(GetCurrentDetectorBuilder()->getGasMat());
    if(!det->getGas()) LOG_ERROR <<"gas not there!"<<endm;
    det->setMaterial(matS);
    //    det->setElossCalculator(ElossCalculator);
    det->setHitErrorCalculator(StiBTofHitErrorCalculator::instance());
    // Adding detector, not sure if setKey is necessary  
    //if(ActiveVolume){
    det->setKey(1, 0);
    det->setKey(2, tray-1);
    add(0, tray-1, det);
    if (_debug) {
      LOG_INFO 
	<< "===>NEW:BTof:pDetector:Name               = " << det->getName() << endm;
      LOG_INFO
	<< " NormalRefAngle    = " << pPlacement->getNormalRefAngle()*TMath::RadToDeg() 
	<< " NormalRadius      = " << pPlacement->getNormalRadius()                     
	<< " NormalYoffset     = " << pPlacement->getNormalYoffset()    << endm;
      LOG_INFO                
	<< " CenterRefAngle    = " << pPlacement->getCenterRefAngle()*TMath::RadToDeg() 
	<< " CenterRadius      = " << pPlacement->getCenterRadius()                     
	<< " CenterOrientation = " << pPlacement->getCenterOrientation()*TMath::RadToDeg() 
	<< " LayerRadius       = " << pPlacement->getLayerRadius()                         
	<< " LayerAngle        = " << pPlacement->getLayerAngle()*TMath::RadToDeg()   << endm;
      LOG_INFO                     
	<< " Zcenter           = " << pPlacement->getZcenter()                             
	<< " Tray               = " << tray                                                 
	<< " Active?            = " << det->isActive() << endm;
    }
  }
}
