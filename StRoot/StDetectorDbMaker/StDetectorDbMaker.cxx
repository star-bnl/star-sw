#include "StMessMgr.h"
#include "StDetectorDbSpaceCharge.h"
#include "St_trigDetSumsC.h"
#include "StDetectorDbMaker.h"
#include "StDetectorDbRichScalers.h"
ClassImp(StDetectorDbMaker)
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Make(){
    // Also need to update instances for classes done in InitRun.
    // This is needed because of a feature in TTable
    // Please ingore unused variables..the call to instane() is needed!
  if (GetMode() != 1) {
    TDataSet *set = GetDataSet("inputStream_DAQ");
    if (set) {
      St_trigDetSums *table = (St_trigDetSums *) set->Find("trigDetSums");
      if (table) {
	LOG_QA << "get trigDetSums from inputStream_DAQ" << endm;
	new St_trigDetSumsC(table);
      }
    }
  }
  if (! St_trigDetSumsC::instance()) {
    St_trigDetSums *table = (St_trigDetSums *) GetDataBase("Calibrations/rich/trigDetSums");
    if (table) {
      LOG_QA << "get trigDetSums from DB Calibrations/rich/trigDetSums" << endm;
      new St_trigDetSumsC(table);
    }
  }
#if 0
  // Jamie Asked for SpaceCharge to be counted every event
  LOG_QA << "Space Charge Correction = " << StDetectorDbSpaceCharge::instance()->getSpaceChargeCoulombs()
	       << " Coulombs" << endm;
  LOG_QA << "Space Charge CorrectionR2 = " << StDetectorDbSpaceChargeR2::instance()->getSpaceChargeCoulombs()
	       << " Coulombs" << endm;
#endif  
  return kStOK;
}
