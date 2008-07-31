/***************************************************************************
 *
 * $Id: StTpcMixerMaker.cxx,v 1.1 2008/07/31 20:45:27 fisyak Exp $
 * $Log: StTpcMixerMaker.cxx,v $
 * Revision 1.1  2008/07/31 20:45:27  fisyak
 * Add TpcMixer
 *
 **************************************************************************/
#include "StTpcMixerMaker.h"
#include "StMessMgr.h" 
#include "StTpcRawData.h"
ClassImp(StTpcMixerMaker)
//______________________________________________________________________________
Int_t StTpcMixerMaker::Make() {
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
  return kStOK;
} // Make() 


