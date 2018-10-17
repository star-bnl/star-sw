#include "StMessMgr.h"
#include "StDetectorDbSpaceCharge.h"
#include "St_trigDetSumsC.h"
#include "StDetectorDbMaker.h"
#include "StDetectorDbRichScalers.h"
#if 0
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#endif
St_trigDetSums* St_trigDetSumsC::fgTableCopy = 0;
Int_t StDetectorDbMaker::_debug = 0;
ClassImp(StDetectorDbMaker)
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Make(){
    // Also need to update instances for classes done in InitRun.
    // This is needed because of a feature in TTable
    // Please ingore unused variables..the call to instane() is needed!

  // Must delete any existing St_trigDetSumsC first, or absence from DAQ stream
  //  in the middle of a file won't properly trigger looking to DB, nor do we
  //  want to keep old table around
  if (GetMode() != 1) {
    TDataSet *set = GetDataSet("inputStream_DAQ");
    if (set) {
      St_trigDetSums *table = (St_trigDetSums *) set->Find("trigDetSums");
      if (table) {
	LOG_QA << "get trigDetSums from inputStream_DAQ" << endm;
	StMaker::GetChain()->AddData(new St_trigDetSumsC(table));
      }
#if 0
    } else {
      StMuDstMaker *MuDstMk = (StMuDstMaker *) StMaker::GetChain()->Maker("MuDst");
      if (MuDstMk) {
	StMuDst *MuDst = MuDstMk->muDst();
	StMuEvent *MuEve = 0;
	if (MuDst) MuEve = MuDst->event();
	if (MuEve) {
	   St_trigDetSums *table = new St_trigDetSums("trigDetSums",1);
	  table->AddAt(&MuEve->trigDetSums());
	  StMaker::GetChain()->AddData(table);
	}
      }
#endif
    }
  }
#if 0
  if (! St_trigDetSumsC::instance()) {
    St_trigDetSums *table = (St_trigDetSums *) GetDataBase("Calibrations/rich/trigDetSums");
    if (table) {
#if 0
      LOG_QA << "get trigDetSums from DB Calibrations/rich/trigDetSums" << endm;
#endif
      StMaker::GetChain()->AddData(new St_trigDetSumsC(table));
    }
  }
#if 0
  // Jamie Asked for SpaceCharge to be counted every event
  LOG_QA << "Space Charge Correction = " << StDetectorDbSpaceCharge::instance()->getSpaceChargeCoulombs()
	       << " Coulombs" << endm;
  LOG_QA << "Space Charge CorrectionR2 = " << StDetectorDbSpaceChargeR2::instance()->getSpaceChargeCoulombs()
	       << " Coulombs" << endm;
#endif  
#endif
  return kStOK;
}
