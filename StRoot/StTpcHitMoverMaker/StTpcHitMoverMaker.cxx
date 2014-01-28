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
#include "StDetectorDbMaker/St_tpcEffectiveGeomC.h"
#include "TMath.h"
ClassImp(StTpcHitMover)
//#define __Move2TpcHalf__
#define __DEBUG__
#ifdef __DEBUG__
#define PrPP(A,B) if (_debug %10 > 1) {LOG_INFO << "StTpcHitMover::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
StTpcCoordinateTransform *StTpcHitMover::mTpcTransForm = 0;
StMagUtilities*   StTpcHitMover::mExB = 0;
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
  if (mExB && mExB->GetSpaceChargeMode() &&
      StDetectorDbSpaceChargeR2::instance()->IsMarked()) {
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
	  Double_t padlength = (io == 0) ? 
	    gStTpcDb->PadPlaneGeometry()->innerSectorPadLength() : 
	    gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
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
		  StTpcPadCoordinate padcoord(sector, row, pad, time);
		  StTpcLocalSectorCoordinate  coorS;
		  transform(padcoord,coorS,kFALSE);
		  StTpcLocalCoordinate  coorL;
		  Double_t y = coorS.position().y();
		  for (Int_t l = 0; l < 3; l++) {// center, upper and lower
		    if      (l == 1) coorS.position().setY(y + padlength/2);
		    else if (l == 2) coorS.position().setY(y - padlength/2);
		    transform(coorS,coorL);
#if 0
		    Double_t scale = 1. + St_tpcEffectiveGeomC::instance()->scale()*coorL.position().y();
		    StTpcDb::instance()->ScaleDriftVelocity(scale);
		    transform(padcoord,coorL,kFALSE);
		    StTpcDb::instance()->ScaleDriftVelocity(1./scale);
#endif
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
  if (mExB) {
    pEvent->runInfo()->setSpaceCharge(mExB->CurrentSpaceChargeR2());
    pEvent->runInfo()->setSpaceChargeCorrectionMode(mExB->GetSpaceChargeMode());
  }
  return kStOK;
}
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StTpcLocalCoordinate &coorLTD) {
  mExB = StMagUtilities::Instance();
#if 0
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  StTpcCoordinateTransform &transform = *mTpcTransForm;
  static StTpcLocalSectorCoordinate  coorLS;
  transform(coorL,coorLS);   PrPP(moveTpcHit,coorL); PrPP(moveTpcHit,coorLS); // to sector 12
  static StTpcPadCoordinate Pad;
  transform(coorLS,Pad,kFALSE,kFALSE); PrPP(moveTpcHit,Pad);
  static StTpcLocalCoordinate  coorLT; // before undo distortions
  transform(coorLS,coorLT); PrPP(moveTpcHit,coorLT);//
  coorLTD = coorLT;          // distortions
#else
  coorLTD = coorL;           // distortions
#endif
#ifdef __Move2TpcHalf__
  StBeamDirection side = east;
  Int_t Sector = coorL.fromSector();
  if (Sector >= 1 && Sector <= 24) {
    if (Sector <= 12) side = west;
  } else {
    if (coorL.position().z() >  0) side = west;
  }
  Double_t posD[3];
  gStTpcDb->TpcHalf(side).MasterToLocal(coorL.position().xyz(),posD);
  // ExB corrections
  Float_t pos[3] = {posD[0], posD[1], posD[2]};
#else  /* ! __Move2TpcHalf__ */
  Float_t pos[3] = {coorLTD.position().x(), coorLTD.position().y(), coorLTD.position().z()};
#endif /* __Move2TpcHalf__ */
  if ( mExB ) {
    Float_t posMoved[3];
    mExB->UndoDistortion(pos,posMoved,coorL.fromSector());   // input pos[], returns posMoved[]
    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
#ifdef __Move2TpcHalf__
    StThreeVector<double> newPosL;
    gStTpcDb->TpcHalf(side).LocalToMaster(newPos.xyz(),newPosL.xyz());
    coorLTD.setPosition(newPosL); 
#else  /* ! __Move2TpcHalf__ */
    coorLTD.setPosition(newPos); 
#endif /* __Move2TpcHalf__ */
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
// $Id: StTpcHitMoverMaker.cxx,v 1.24 2014/01/28 17:10:39 genevb Exp $
// $Log: StTpcHitMoverMaker.cxx,v $
// Revision 1.24  2014/01/28 17:10:39  genevb
// Fill otherwise empty SpaceCharge info in StRunInfo
//
// Revision 1.23  2014/01/08 21:14:28  fisyak
// Add transformations for Upper and Lower tpc hits postions (new dX calculation in dE/dx)
//
