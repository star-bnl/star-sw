#include "StTpcHitMoverMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StMessMgr.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StEventTypes.h"
#include "TMath.h"
ClassImp(StTpcHitMover)
//________________________________________________________________________________
StTpcHitMover::StTpcHitMover(const Char_t *name) : StMaker(name),
						   mTpcTransForm(0), mExB(NULL) {
  gMessMgr->Info("StTpcHitMover::StTpcHitMover: constructor called");
}
//________________________________________________________________________________
StTpcHitMover::~StTpcHitMover() {
  FlushDB();
}
//________________________________________________________________________________
Int_t StTpcHitMover::Init() {
  return StMaker::Init();
}
//________________________________________________________________________________
Int_t StTpcHitMover::InitRun(Int_t runnumber) {
  FlushDB();
  return kStOk;
}
//________________________________________________________________________________
void StTpcHitMover::FlushDB() {
  SafeDelete(mTpcTransForm);
}
//________________________________________________________________________________
Int_t StTpcHitMover::Make() {
  static StGlobalCoordinate    coorG;
  Bool_t EmbeddingShortCut = IAttr("EmbeddingShortCut");
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if ( pEvent) {
    gMessMgr->Info() << "StTpcHitMover::Make use StEvent " << endm;
    if (! gStTpcDb) {
      gMessMgr->Error() << "StTpcHitMover::Make TpcDb has not been instantiated " << endm;
      return kStErr;
    }
    StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
    if (TpcHitCollection) {
      UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
      for (UInt_t i = 0; i< numberOfSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (sectorCollection) {
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (rowCollection) {
	      StSPtrVecTpcHit &hits = rowCollection->hits();
	      UInt_t NoHits = hits.size();
	      if (NoHits) {
		for (UInt_t k = 0; k < NoHits; k++) {
		  StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		  if (EmbeddingShortCut && tpcHit->idTruth() && tpcHit->idTruth() < 10000 && 
		      tpcHit->qaTruth() > 95) continue; // don't move embedded hits
		  StTpcLocalCoordinate  coorL(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z(),i+1,j+1);
		  moveTpcHit(coorL,coorG);

                  if (mExB && mExB->GetSpaceChargeMode() &&
                    StDetectorDbSpaceChargeR2::instance()->IsMarked()) {
                    gMessMgr->Error() << "StTpcHitMover::Make questionable hit corrections" << endm;
                    return kStSkip;
                  }

		  StThreeVectorF xyzF(coorG.position().x(),coorG.position().y(),coorG.position().z());
		  tpcHit->setPosition(xyzF);
		}
	      }
	    }
	  }
	}
      }
    }
    return kStOK;
  } 
  else {
    LOG_WARN << "StTpcHitMover::Make there is no StEvent " << endm;
    return kStWarn;
  }
}
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StGlobalCoordinate &coorG) {
  mExB = StMagUtilities::Instance();
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;
  static StTpcLocalSectorCoordinate  coorLS;
  transform(coorL,coorLS);   // to sector 12
  static StTpcLocalSectorAlignedCoordinate  coorLSA;
  transform(coorLS,coorLSA); // alignment
  static StTpcLocalCoordinate  coorLT;
  transform(coorLSA,coorLT); //
  static StTpcLocalCoordinate  coorLTD;
  coorLTD = coorLT;          // distortions
  // ExB corrections
  Float_t pos[3] = {coorLTD.position().x(),coorLTD.position().y(),coorLTD.position().z()};
  if ( mExB ) {
    Float_t posMoved[3];
    mExB->UndoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
    coorLTD.setPosition(newPos);
  }
  transform(coorLTD,coorG);
}
