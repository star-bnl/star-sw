////////////////////////////////////////////////////////////////////////////
//                                                                        //
//  StEvtTrigDetSumsMaker populates the TrigDetSums table from StEvent.   //
//  This allows the table to be filled with the same information when     //
//  reading event.root files that was available when the files were       //
//  created. In particular, database values of RICH scalers are slightly  //
//  different. Using StEvtTrigDetSumsMaker also avoids the unnecessary    //
//  overhead of going to the database for information already available.  //
//                                                                        //
//  Note that trigDetSums is used by StDetectorDbRichScalers, which in    //
//  turn is used by St_spaceChargeCorC, used by StMagUtilities. It        //
//  should be placed before any use of StMagUtilities in the chain.       //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StEvtTrigDetSumsMaker.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StEventTypes.h"

St_trigDetSums* St_trigDetSumsC::fgTableCopy = 0;

ClassImp(StEvtTrigDetSumsMaker)
//_____________________________________________________________________________
Int_t StEvtTrigDetSumsMaker::Make(){

  // Get StEvent and related info, determine if things are OK
  StEvent* event = (StEvent*) GetInputDS("StEvent");
  if (!event) {
    LOG_ERROR << "StEvtTrigDetSumsMaker: no StEvent" << endm;
    return kStErr;
  } 
  if (event->Find("trigDetSums")) {
    LOG_WARN << "StEvtTrigDetSumsMaker: StEvent already contains trigDetSums...skipping" << endm;
    return kStOK;
  }
  StRunInfo* runInfo = event->runInfo();
  if (!runInfo) {
    LOG_ERROR << "StEvtTrigDetSumsMaker: no runInfo" << endm;
    return kStErr;
  }

  delete St_trigDetSumsC::instance();
  St_trigDetSums* table = new St_trigDetSums("trigDetSums",1);
  trigDetSums_st* row = table->GetTable();

  row->zdcX = runInfo->zdcCoincidenceRate();
  row->zdcWest = runInfo->zdcWestRate();
  row->zdcEast = runInfo->zdcWestRate();
  row->bbcX = runInfo->bbcCoincidenceRate();
  row->bbcWest = runInfo->bbcWestRate();
  row->bbcEast = runInfo->bbcWestRate();
  row->bbcYellowBkg = runInfo->bbcYellowBackgroundRate();
  row->bbcBlueBkg = runInfo->bbcBlueBackgroundRate();
  row->L0 = runInfo->l0RateToRich();

  table->SetNRows(1);
  new St_trigDetSumsC(table);
  return kStOK;
}

//_____________________________________________________________________________
// $Id: StEvtTrigDetSumsMaker.cxx,v 1.3 2015/06/26 21:38:00 genevb Exp $
// $Log: StEvtTrigDetSumsMaker.cxx,v $
// Revision 1.3  2015/06/26 21:38:00  genevb
// Not necessary when trigDetSums is already available from StEvent
//
// Revision 1.2  2012/10/15 17:38:34  genevb
// Add CVS logging
//
//

