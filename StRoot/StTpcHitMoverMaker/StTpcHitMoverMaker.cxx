#include "StTpcHitMoverMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StMessMgr.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StEventTypes.h"
#ifdef __CORRECT_CHARGE__
#include "StDetectorDbMaker/St_tss_tssparC.h"
#endif /* __CORRECT_CHARGE__ */
#include "StDetectorDbMaker/St_tpcSlewingC.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "TMath.h"
ClassImp(StTpcHitMover)
#define __DEBUG__
#ifdef __DEBUG__
#define PrPP(A,B) if (Debug()%10 > 1) {LOG_INFO << "StTpcHitMover::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
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
  if (! pEvent) {
    LOG_WARN << "StTpcHitMover::Make there is no StEvent " << endm;
    return kStWarn;
  }
  gMessMgr->Info() << "StTpcHitMover::Make use StEvent " << endm;
  if (! gStTpcDb) {
    gMessMgr->Error() << "StTpcHitMover::Make TpcDb has not been instantiated " << endm;
    return kStErr;
  }
  static Int_t NoInnerPadRows = St_tpcPadPlanesC::instance()->innerPadRows();
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
#ifdef __CORRECT_CHARGE__
  St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  Double_t gains[2] = {
    tsspar->gain_in() / tsspar->wire_coupling_in() *tsspar->ave_ion_pot() *tsspar->scale(),
    tsspar->gain_out()/ tsspar->wire_coupling_out()*tsspar->ave_ion_pot() *tsspar->scale()
  };
#endif /* __CORRECT_CHARGE__ */
  if (TpcHitCollection) {
    UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
    for (UInt_t i = 0; i< numberOfSectors; i++) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t sector = i + 1;
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (int j = 0; j< numberOfPadrows; j++) {
	  Int_t row = j + 1;
	  Int_t io = 0;
	  if (row > NoInnerPadRows) io = 1;
	  StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	  if (rowCollection) {
	    StSPtrVecTpcHit &hits = rowCollection->hits();
	    UInt_t NoHits = hits.size();
	    if (NoHits) {
	      for (UInt_t k = 0; k < NoHits; k++) {
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
#ifdef __CORRECT_CHARGE__
		Double_t q = tpcHit->charge();
		PrPP(Make,*tpcHit);
		if (tpcHit->adc()) { // correct charge
		  q = gains[io] * ((Double_t) tpcHit->adc());
		}
#endif /* __CORRECT_CHARGE__ */
		if (EmbeddingShortCut && tpcHit->idTruth() && tpcHit->idTruth() < 10000 && 
		    tpcHit->qaTruth() > 95) {
		  continue; // don't move embedded hits
		}
		if (! tpcHit->pad() && ! tpcHit->timeBucket()) {// old style, no pad and timeBucket set
		  StTpcLocalCoordinate  coorL(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z(),i+1,j+1);
		  moveTpcHit(coorL,coorG);
		} else { //  transoform from original pad and time bucket measurements
		  Float_t pad  = tpcHit->pad();
		  Float_t time = tpcHit->timeBucket();
		  StTpcPadCoordinate padcoord(sector, row, pad, time);
		  StTpcLocalCoordinate  coorL;
		  transform(padcoord,coorL,kFALSE);
		  moveTpcHit(coorL,coorG);
		}
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
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StGlobalCoordinate &coorG) {
  mExB = StMagUtilities::Instance();
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;
  static StTpcLocalSectorCoordinate  coorLS;
  transform(coorL,coorLS);   PrPP(moveTpcHit,coorL); PrPP(moveTpcHit,coorLS); // to sector 12
#if 0
  static StTpcPadCoordinate Pad;
  transform(coorLS,Pad,kFALSE,kFALSE); PrPP(moveTpcHit,Pad);
#endif
  static StTpcLocalCoordinate  coorLT; // before undo distortions
  transform(coorLS,coorLT); PrPP(moveTpcHit,coorLT);//
  static StTpcLocalCoordinate  coorLTD; // after undo distortions
  coorLTD = coorLT;          // distortions
  // ExB corrections
  Float_t pos[3] = {(Float_t) coorLTD.position().x(),
		    (Float_t) coorLTD.position().y(),
		    (Float_t) coorLTD.position().z()};
  if ( mExB ) {
    Float_t posMoved[3];
    mExB->UndoDistortion(pos,posMoved,coorL.fromSector());   // input pos[], returns posMoved[]
    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
    coorLTD.setPosition(newPos); 
  }
  transform(coorLTD,coorG); PrPP(moveTpcHit,coorLTD); PrPP(moveTpcHit,coorG); 
}
