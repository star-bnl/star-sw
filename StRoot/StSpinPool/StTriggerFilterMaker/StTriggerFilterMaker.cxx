// $Id: StTriggerFilterMaker.cxx,v 1.2 2015/09/09 20:29:39 akio Exp $

#include "StTriggerFilterMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

ClassImp(StTriggerFilterMaker)

StTriggerFilterMaker::StTriggerFilterMaker(const char *name) : StMaker(name), mPrint(0) { }

StTriggerFilterMaker::~StTriggerFilterMaker() {
    mGoodTriggers.clear();
    mVetoTriggers.clear();
}

Int_t StTriggerFilterMaker::Init() {
    // this allows us to skip an event for other Makers
    SetAttr(".Privilege",1);
    return kStOk;
}

Int_t StTriggerFilterMaker::Make() {
  if(mPrint){
    vector<unsigned int> ids = StMuDst::event()->triggerIdCollection().nominal().triggerIds();
    LOG_INFO << "Offline Trigger Id = ";
    for(unsigned i=0; i<ids.size(); ++i) LOG_INFO << ids[i] << " ";
  }
  for(unsigned i=0; i<mVetoTriggers.size(); ++i) {
    if(StMuDst::event()->triggerIdCollection().nominal().isTrigger(mVetoTriggers[i])) {
      if(mPrint)  LOG_INFO << " Veto" << endm;
      return kStSkip;
    }
  }
  for(unsigned i=0; i<mGoodTriggers.size(); ++i) {
    if(StMuDst::event()->triggerIdCollection().nominal().isTrigger(mGoodTriggers[i])) {
      if(mPrint)   LOG_INFO << " Accept" << endm;
      return kStOk;
    }
  }
  if(mPrint)    LOG_INFO << " Skip" << endm;
  return kStSkip;
}

/*****************************************************************************
 * $Log: StTriggerFilterMaker.cxx,v $
 * Revision 1.2  2015/09/09 20:29:39  akio
 * Adding Vetoing TriggerId
 * Also adding printing if option is set
 *
 * Revision 1.1  2008/01/23 04:45:07  kocolosk
 * Privileged Maker which skips events unless they fired any one of a set of supplied trigIDs
 *
 *****************************************************************************/
