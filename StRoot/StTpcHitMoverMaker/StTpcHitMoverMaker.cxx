#include "StTpcHitMoverMaker.h"

#include "StMessMgr.h"

#include "TTableIter.h"
#include "tables/St_type_index_Table.h"

#include "tpc/St_tcl_Module.h"
#include "tpc/St_tpt_Module.h"

#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StSectorAligner.h"
#include "StDbUtilities/StCoordinates.hh"

StTpcHitMover::StTpcHitMover(const Char_t *name) : StMaker(name), 
						   mAlignSector(kTRUE),
						   mSectorAligner(NULL),
						   mExB(NULL) {
  setInputDataSetName("tpc_hits");
  setInputHitName("tphit");
  setOutputMode(0);
}

StTpcHitMover::~StTpcHitMover() {
  if (mSectorAligner) {
    delete mSectorAligner;
  }
  if (mExB) {
    delete mExB;
  }
}

Int_t StTpcHitMover::Init() {
  Char_t buffer[1024];
  snprintf(buffer,1023,"StTpcHitMover::Init() - reading hits from %s:%s\n",
	   getInputDataSetName().Data(),getInputHitName().Data());
  buffer[1023] = '\0';
  gMessMgr->Info(buffer);
  snprintf(buffer,1023,"StTpcHitMover::Init() - sector align: %d\n",
	   mAlignSector);
  buffer[1023] = '\0';
  gMessMgr->Info(buffer);
  snprintf(buffer,1023,"StTpcHitMover::Init() - ExB corrections: %d\n",
	   (m_Mode & 0x01));
  buffer[1023] = '\0';
  gMessMgr->Info(buffer);
  snprintf(buffer,1023,"StTpcHitMover::Init() - mag utils options: 0x%X\n",
	   ((m_Mode & 0x3FFE) >> 1));
  return StMaker::Init();
}

Int_t StTpcHitMover::InitRun(Int_t runnumber) {
  // delete StMagUtilities just to be sure...
  if (mExB) {
    delete mExB;
  }
  return kStOk;
}

Int_t StTpcHitMover::Make() {
  St_DataSet *tpc_data = GetInputDS(mInputDataSetName);
  if (!tpc_data) {
    // no TPC ???
    return kStOk;
  }
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit *tphit = (St_tcl_tphit *) gime(mInputHitName);
  if (!tphit) {
    // no cluster data?
    return kStWarn;
  }
  
  tcl_tphit_st* spc = tphit->GetTable();
  if (!spc) {
    return kStWarn;
  }

  Float_t x[3];
  Float_t xprime[3];
  for (Int_t i = 0; i < tphit->GetNRows(); i++, spc++) {
    short sector = short(spc->row/100.);
    short row = spc->row-sector*100;
    x[0] = spc->x;
    x[1] = spc->y;
    x[2] = spc->z;

    moveTpcHit(x,xprime,sector,row);

    if (mOutputMode == 0) {
      spc->x = xprime[0];
      spc->y = xprime[1];
      spc->z = xprime[2];
    }
  }

  return kStOk;
}

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

void StTpcHitMover::moveTpcHit(Float_t pos[3], Float_t posMoved[3],
			       Short_t sector, Short_t row) {
  // initialize variables
  posMoved[0] = pos[0];
  posMoved[1] = pos[1];
  posMoved[2] = pos[2];

  // align sector
  if (mAlignSector) { 
    mSectorAligner = new StSectorAligner(gStTpcDb);
    mSectorAligner->moveHit(pos,posMoved,sector,row);
    pos[0] = posMoved[0];
    pos[1] = posMoved[1];
    pos[2] = posMoved[2];
    delete mSectorAligner;
  }

  // ExB corrections
  if (m_Mode & 0x01) {
    // option handling needs some clean up, but right now we stay compatible
    Int_t option = (m_Mode & 0x3FFE) >> 1;
    if (mExB == 0) {
      TDataSet *RunLog = GetDataBase("RunLog");
      mExB = new StMagUtilities(gStTpcDb, RunLog, option);
    }
    mExB->UndoDistortion(pos,posMoved);
    pos[0] = posMoved[0];
    pos[1] = posMoved[1];
    pos[2] = posMoved[2];
  }

  // transformation to global coordinates
  StTpcCoordinateTransform transform(gStTpcDb);

  StTpcLocalCoordinate local(pos[0],pos[1],pos[2]);
  StGlobalCoordinate global;
  transform(local,global);

  posMoved[0] = global.position().x();
  posMoved[1] = global.position().y();
  posMoved[2] = global.position().z();
} 

ClassImp(StTpcHitMover)
