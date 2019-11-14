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
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcEffectiveGeomC.h"
#include "StDetectorDbMaker/St_tpcTimeBucketCorC.h"
#include "StDetectorDbMaker/St_spaceChargeCorC.h"
#include "StDetectorDbMaker/St_tpcChargeEventC.h"
#include "StDetectorDbMaker/St_tpcBXT0CorrEPDC.h"
#include "TMath.h"
ClassImp(StTpcHitMover)
#define __DEBUG__
#ifdef __DEBUG__
#define PrPP(A,B) if (_debug %10 > 1) {LOG_INFO << "StTpcHitMover::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
StTpcCoordinateTransform *StTpcHitMover::mTpcTransForm = 0;
Int_t StTpcHitMover::_debug  = 0;
//________________________________________________________________________________
StTpcHitMover::StTpcHitMover(const Char_t *name) : StMaker(name) {
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
  if (StMagUtilities::Instance() && StMagUtilities::Instance()->GetSpaceChargeMode() &&
      St_spaceChargeCorR2C::instance()->IsMarked()) {
    gMessMgr->Error() << "StTpcHitMover::Make questionable hit corrections" << endm;
    return kStSkip;
  }
  static StGlobalCoordinate    coorG;
  Bool_t EmbeddingShortCut = IAttr("EmbeddingShortCut");
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StTpcHitMover::Make there is no StEvent " << endm;
    return kStWarn;
  }
//	EPD based event-by-event correction for the hit timing
	double mTimeBinWidth = 1./StTpcDb::instance()->Electronics()->samplingFrequency();

	int ew = 0;
        int TAC = 0;
	int maxTAC = -1;

	int doEPDT0Correction = StTpcBXT0CorrEPDC::instance()->nrows();

	if (doEPDT0Correction) {
		StEpdCollection * epdCol = pEvent->epdCollection();
		if (epdCol) {
			StSPtrVecEpdHit &epdHits = epdCol->epdHits();
			int nEpdHits = epdHits.size();

			for(int i = 0; i < nEpdHits; i++) {
				StEpdHit * epdHit = dynamic_cast<StEpdHit*>(epdHits[i]);
				TAC = 0;
				if (epdHit->tile() > 9)  continue; // only tiles 1 - 9 have timing info
				if (epdHit->id() < 0) ew = -1; // tile is on the east
				else ew = 1;
				if (epdHit->adc() < 100) continue;
				TAC = epdHit->tac(); // this is the timing
				if (TAC > maxTAC) maxTAC = TAC;
			}
		}
	}
//	======================================================

  gMessMgr->Info() << "StTpcHitMover::Make use StEvent " << endm;
  if (! gStTpcDb) {
    gMessMgr->Error() << "StTpcHitMover::Make TpcDb has not been instantiated " << endm;
    return kStErr;
  }
  if (pEvent && StMagUtilities::Instance() && StMagUtilities::Instance()->UsingDistortion(kAbortGap)) {
    StTriggerData* trg = pEvent->triggerData();
    if (trg) St_tpcChargeEventC::instance()->findChargeTimes(trg->bunchCounter());
  }
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

	double driftVelocity = StTpcDb::instance()->DriftVelocity(sector);

	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (int j = 0; j< numberOfPadrows; j++) {
	  Int_t row = j + 1;
	  Int_t io = 0;
	  if (row > St_tpcPadConfigC::instance()->innerPadRows(sector)) io = 1;
	  Double_t padlength = (io == 0) ? 
	    St_tpcPadConfigC::instance()->innerSectorPadLength(sector) : 
	    St_tpcPadConfigC::instance()->outerSectorPadLength(sector);
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
		  StThreeVectorF xyzF(coorG.position().x(),coorG.position().y(),coorG.position().z());
		  tpcHit->setPosition(xyzF);
		} else { //  transoform from original pad and time bucket measurements
		  Float_t pad  = tpcHit->pad();
		  Float_t time = tpcHit->timeBucket();
		  if (! StTpcDb::IsOldScheme()) {
		    if (St_tpcTimeBucketCorC::instance()->getNumRows()) {
		      Int_t io = 0;
		      if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) io = 1;
		      Double_t noTmbks = tpcHit->maxTmbk() - tpcHit->minTmbk() + 1;
		      time += St_tpcTimeBucketCorC::instance()->CalcCorrection(io, noTmbks);
		    }
		  }
//		THIS IS A BLOCK TO CORRECT TIMING IN FXT MODE FOR DATA
		if (doEPDT0Correction) time += StTpcBXT0CorrEPDC::instance()->getCorrection(maxTAC, driftVelocity, mTimeBinWidth);
//		======================================================

		  StTpcPadCoordinate padcoord(sector, row, pad, time);
		  StTpcLocalSectorCoordinate  coorS;
		  transform(padcoord,coorS,kFALSE);
		  StTpcLocalCoordinate  coorL;
		  Double_t y = coorS.position().y();
		  for (Int_t l = 0; l < 3; l++) {// center, upper and lower
		    if      (l == 1) coorS.position().setY(y + padlength/2);
		    else if (l == 2) coorS.position().setY(y - padlength/2);
		    transform(coorS,coorL);
		    moveTpcHit(coorL,coorG);
		    StThreeVectorF xyzF(coorG.position().x(),coorG.position().y(),coorG.position().z());
		    if      (l == 1) tpcHit->setPositionU(xyzF);
		    else if (l == 2) tpcHit->setPositionL(xyzF);
		    else             tpcHit->setPosition (xyzF);
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  if (StMagUtilities::Instance()) {
    pEvent->runInfo()->setSpaceCharge(StMagUtilities::Instance()->CurrentSpaceChargeR2());
    pEvent->runInfo()->setSpaceChargeCorrectionMode(StMagUtilities::Instance()->GetSpaceChargeMode());
  }
  return kStOK;
}
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StTpcLocalCoordinate &coorLTD) {
  coorLTD = coorL;           // distortions
  Float_t pos[3] = {(Float_t) coorLTD.position().x(), (Float_t) coorLTD.position().y(), (Float_t) coorLTD.position().z()};
  if ( StMagUtilities::Instance() ) {
    Float_t posMoved[3];
    StMagUtilities::Instance()->UndoDistortion(pos,posMoved,coorL.fromSector());   // input pos[], returns posMoved[]
    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
    coorLTD.setPosition(newPos); 
  }
}
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StGlobalCoordinate &coorG) {
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;
  static StTpcLocalCoordinate  coorLTD;
  moveTpcHit(coorL,coorLTD);
  transform(coorLTD,coorG); PrPP(moveTpcHit,coorLTD); PrPP(moveTpcHit,coorG); 
}
// $Id: StTpcHitMoverMaker.cxx,v 1.32 2019/11/14 23:07:48 iraklic Exp $
// $Log: StTpcHitMoverMaker.cxx,v $
// Revision 1.32  2019/11/14 23:07:48  iraklic
// added timebucket and drift velocity into epd-based T0 correction calculation
//
// Revision 1.30  2019/04/22 20:47:16  genevb
// Introducing codes for AbortGapCleaning distortion corrections
//
// Revision 1.29  2018/06/07 04:48:28  genevb
// Explicit include for spaceChargeCor needed
//
// Revision 1.28  2018/04/11 02:43:22  smirnovd
// Enable TPC/iTPC switch via St_tpcPadConfig
//
// This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
// A sector ID is passed to St_tpcPadConfig in order to extract parameters for
// either TPC or iTPC
//
// Revision 1.27  2014/07/27 13:23:09  fisyak
// Add cast for c++11 option
//
// Revision 1.26  2014/06/27 14:45:51  fisyak
// Add swith between new and old schema, clean up
//
// Revision 1.25  2014/06/26 21:32:25  fisyak
// New Tpc Alignment, v632
//
// Revision 1.24  2014/01/28 17:10:39  genevb
// Fill otherwise empty SpaceCharge info in StRunInfo
//
// Revision 1.23  2014/01/08 21:14:28  fisyak
// Add transformations for Upper and Lower tpc hits postions (new dX calculation in dE/dx)
//
