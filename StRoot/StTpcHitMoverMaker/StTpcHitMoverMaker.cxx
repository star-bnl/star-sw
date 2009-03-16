#include "StTpcHitMoverMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StMessMgr.h"
#include "tables/St_tcl_tphit_Table.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StEventTypes.h"
#include "TMath.h"
ClassImp(StTpcHitMover)
//________________________________________________________________________________
StTpcHitMover::StTpcHitMover(const Char_t *name) : StMaker(name),
						   mAlignSector(kFALSE),
						   mExB(NULL), mTpcTransForm(0) {
  gMessMgr->Info("StTpcHitMover::StTpcHitMover: constructor called");
  setInputDataSetName("tpc_hits");
  setInputHitName("tphit");
  setOutputMode(0);
}
//________________________________________________________________________________
StTpcHitMover::~StTpcHitMover() {
  FlushDB();
}
//________________________________________________________________________________
Int_t StTpcHitMover::Init() {

  TString giHN  = getInputHitName();
  TString giDSn = getInputDataSetName();

  gMessMgr->Info() << "StTpcHitMover::Init() - reading hits from "  <<
    giDSn << ":" << giHN << endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - sector align:     "  <<
    mAlignSector   << endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - ExB corrections:  " <<
    (TMath::Abs(m_Mode) & 0x01)<< endm;

  gMessMgr->Info() << "StTpcHitMover::Init() - mag utils options " <<
    ((TMath::Abs(m_Mode) & 0x3FFE) >> 1) << endm;

  return StMaker::Init();
}
//________________________________________________________________________________
Int_t StTpcHitMover::InitRun(Int_t runnumber) {
  FlushDB();
  return kStOk;
}
//________________________________________________________________________________
void StTpcHitMover::FlushDB() {
  SafeDelete(mExB);
  SafeDelete(mTpcTransForm);
}
//________________________________________________________________________________
Int_t StTpcHitMover::Make() {
  if (TMath::Abs(m_Mode) & 0x01) {
    // option handling needs some clean up, but right now we stay compatible
    Int_t option = (TMath::Abs(m_Mode) & 0x7FFFFFFE) >> 1;
    if (! mExB ) {
      TDataSet *RunLog = GetDataBase("RunLog");
      mExB = new StMagUtilities(gStTpcDb, RunLog, option);
    }
  }
  if (! mTpcTransForm) mTpcTransForm = new StTpcCoordinateTransform(gStTpcDb);
  static StGlobalCoordinate    coorG;
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
		  if (m_Mode < 0 && tpcHit->idTruth() && tpcHit->idTruth() < 10000 && 
		      tpcHit->qaTruth() > 95) continue; // don't move embedded hits
		  StTpcLocalCoordinate  coorL(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z(),i+1,j+1);
		  moveTpcHit(coorL,coorG);
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

  St_DataSet *tpc_data = GetInputDS(mInputDataSetName);
  if (!tpc_data){
    // no TPC ???
    gMessMgr->Warning() << "StTpcHitMover::Make: no " << mInputDataSetName << endm;
    return kStOk;
  }

  //tpc_data->ls(4);
  //(void) printf("DEBUG1 :: [%s]\n",mInputHitName.Data());

  St_DataSetIter gime(tpc_data);
  St_tcl_tphit *tphit = (St_tcl_tphit *) gime(mInputHitName);
  if (!tphit){
    // no cluster data?
    gMessMgr->Warning() << "StTpcHitMover::Make: no " << mInputHitName     << endm;
    return kStWarn;
  }

  tcl_tphit_st* spc = tphit->GetTable();
  if (!spc)      return kStWarn;

  gMessMgr->Info() << "StTpcHitMover::Make: Input hit table size is " <<
    (int) tphit->GetNRows() << " m_Mode=" << m_Mode << endm;

  Float_t x[3];
  Float_t xprime[3];
  for (Int_t i = 0; i < tphit->GetNRows(); i++, spc++) {
    if (m_Mode < 0 && spc->id_simtrk && spc->id_quality > 95) continue; // don't move embedded hits
    short sector = short(spc->row/100.);
    short row = spc->row-sector*100;
    x[0] = spc->x;
    x[1] = spc->y;
    x[2] = spc->z;
    StTpcLocalCoordinate  coorL(x[0],x[1],x[2],sector,row);
    moveTpcHit(coorL,coorG);
    StThreeVectorF xyzF(coorG.position().x(),coorG.position().y(),coorG.position().z());
    if (mOutputMode == 0) {
      spc->x = coorG.position().x();
      spc->y = coorG.position().y();
      spc->z = coorG.position().z();
    }
  }
  return kStOk;
}
//________________________________________________________________________________
Int_t StTpcHitMover::Finish() {
  return kStOk;
}

void StTpcHitMover::setInputDataSetName(const Char_t *inputDataSetName) {
  if (inputDataSetName) {
    mInputDataSetName = inputDataSetName;
  }
}

void StTpcHitMover::setInputHitName(const Char_t *inputHitName) {
  if (inputHitName) {
    mInputHitName = inputHitName;
  }
}

void StTpcHitMover::setOutputMode(Int_t mode) {
  /*!
    Controls output options:
    0 == tpt compatible mode (overwrite table)
    1 == jeromes risky StEvent fill mode (fill hits directly into StEvent)
  */
  if ((mode >= 0) && (mode <= 1)) {
    mOutputMode = mode;
  }
}
//________________________________________________________________________________
void StTpcHitMover::moveTpcHit(StTpcLocalCoordinate  &coorL,StGlobalCoordinate &coorG) {
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
