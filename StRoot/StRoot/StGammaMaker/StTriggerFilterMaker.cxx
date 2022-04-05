// $Id: StTriggerFilterMaker.cxx,v 1.1 2008/06/11 20:55:36 pibero Exp $

#include "StTriggerFilterMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

ClassImp(StTriggerFilterMaker)

StTriggerFilterMaker::StTriggerFilterMaker(const char *name) : StMaker(name) { }

StTriggerFilterMaker::~StTriggerFilterMaker() {
    mGoodTriggers.clear();
}

Int_t StTriggerFilterMaker::Init() {
    // this allows us to skip an event for other Makers
    SetAttr(".Privilege",1);
    return kStOk;
}

Int_t StTriggerFilterMaker::Make() {
    for(unsigned i=0; i<mGoodTriggers.size(); ++i) {
        if(StMuDst::event()->triggerIdCollection().nominal().isTrigger(mGoodTriggers[i])) {
            return kStOk;
        }
    }
    return kStSkip;
}

/*****************************************************************************
 * $Log: StTriggerFilterMaker.cxx,v $
 * Revision 1.1  2008/06/11 20:55:36  pibero
 * Adam's maker to select events with specific trigger id's.
 *
 * Revision 1.1  2008/01/23 04:45:07  kocolosk
 * Privileged Maker which skips events unless they fired any one of a set of supplied trigIDs
 *
 *****************************************************************************/
