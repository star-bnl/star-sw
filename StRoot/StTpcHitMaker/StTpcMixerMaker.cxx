/***************************************************************************
 *
 * $Id: StTpcMixerMaker.cxx,v 1.2 2012/03/20 15:43:30 fisyak Exp $
 * $Log: StTpcMixerMaker.cxx,v $
 * Revision 1.2  2012/03/20 15:43:30  fisyak
 * Ignore  Input1/2 setting, find Event data sets from chain in order which they have been instantiated, bug 2299
 *
 * Revision 1.1  2008/07/31 20:45:27  fisyak
 * Add TpcMixer
 *
 **************************************************************************/
#include "StTpcMixerMaker.h"
#include "StMessMgr.h" 
#include "StTpcRawData.h"
#include "TSystem.h"
ClassImp(StTpcMixerMaker)
//______________________________________________________________________________
Int_t StTpcMixerMaker::Make() {
#if 0
  static const char *input[2] = {"Input1","Input2"};
  TObjectSet *eventDAQ = (TObjectSet *) GetDataSet("Input1");
  if (! eventDAQ ) {
    LOG_WARN << "No Tpc DAQ Event on Input1" << endm;
    return kStWarn;
  }
  StTpcRawData *rawDAQ = (StTpcRawData *) eventDAQ->GetObject();
  if (! rawDAQ ) {
    LOG_WARN << "No Tpc DAQ Raw on Input1" << endm;
    return kStWarn;
  }
  TObjectSet *eventTRS = (TObjectSet *) GetDataSet("Input2");
  if (! eventTRS ) {
    LOG_WARN << "No Tpc TRS Event on Input2" << endm;
    return kStWarn;
  }
  StTpcRawData *rawTRS = (StTpcRawData *) eventTRS->GetObject();
  if (! rawTRS ) {
    LOG_WARN << "No Tpc TRS Raw on Input2" << endm;
    return kStWarn;
  }
  *rawDAQ += *rawTRS;
#else
  StTpcRawData *inputs[2] = {0, 0};
  TDataSetIter next(StMaker::GetTopChain(),9999);
  TDataSet *set = 0;
  Int_t nfound = 0;
  while (( set = next()) && nfound < 2) {//loop over makers/data sets 
    TString Path(gSystem->BaseName(set->Path()));
    if (Path != "Event") continue;
    TObjectSet *event = dynamic_cast<TObjectSet *>(set);
    if (! event) continue;
    inputs[nfound] = dynamic_cast<StTpcRawData *>(event->GetObject());
    if (! inputs[nfound]) continue;
    LOG_INFO << "Found \t" << Path << " in " << set->Path() << "\tas Input" << nfound+1 << endm;
    nfound++;
  }
  assert(nfound==2);
  *inputs[0] += *inputs[1];
#endif
  return kStOK;
} // Make() 


