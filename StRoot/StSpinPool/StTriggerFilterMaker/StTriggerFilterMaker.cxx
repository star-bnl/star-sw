// $Id: StTriggerFilterMaker.cxx,v 1.3 2021/04/19 18:49:54 akio Exp $

#include "StTriggerFilterMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StTriggerData.h"
#include <functional>

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
    StEvent* event= (StEvent*)GetInputDS("StEvent");
    vector<unsigned int> triggerIds;
    std::function<bool(unsigned int)> isTrigger;
    const StTriggerData* trgdata;
    if(event){
	StTriggerIdCollection* trgIdColl = event->triggerIdCollection();
	triggerIds = trgIdColl->nominal()->triggerIds();
	isTrigger = std::bind(&StTriggerId::isTrigger, trgIdColl->nominal(), std::placeholders::_1);
	trgdata = event->triggerData();
	
    }else if(StMuDst::event()){
	StMuTriggerIdCollection& trgIdColl = StMuDst::event()->triggerIdCollection();
	triggerIds = trgIdColl.nominal().triggerIds();
	isTrigger = std::bind(&StTriggerId::isTrigger, &trgIdColl.nominal(), std::placeholders::_1);
	trgdata = StMuDst::event()->triggerData();
    }else{
	LOG_INFO << "No StEvent nor Mudst" << endm;
	return kStSkip;
    }
    if(mPrint){
	LOG_INFO << "Offline Trigger Id = ";
	for(unsigned int id : triggerIds) LOG_INFO << id << " ";
	LOG_INFO << endm;
    }    
    if(mTofUpperLimit>0){
	if(!trgdata){
	    LOG_INFO << "No TriggerData found but needed for TOF cut" << endm;
	    return kStSkip;
	}
	unsigned int tof = trgdata->tofMultiplicity();  
	if(tof >= mTofUpperLimit){
	    LOG_INFO << Form(" Rejected with TOFmult uppter limit TOFM=%d < UpperLimit=%d",
			     tof,mTofUpperLimit) << endm;
	    return kStSkip;
	}
    }
    for(unsigned int id_veto : mVetoTriggers) {
	if(isTrigger(id_veto)){
	    if(mPrint) LOG_INFO << " Vetoed by id= " << id_veto << endm;
	    return kStSkip;
	}
    }
    for(unsigned int id_good : mGoodTriggers){
	if(isTrigger(id_good)){      
	    if (mPrint) LOG_INFO << " Accepted with id=" << id_good << endm;
	    return kStOk;
	}
    }
    if(mPrint) LOG_INFO << "Skip (no good triggerId)" << endm;
    return kStSkip;
}

/*****************************************************************************
 * $Log: StTriggerFilterMaker.cxx,v $
 * Revision 1.3  2021/04/19 18:49:54  akio
 * Getting info from StEvent as well when not reading from Mudst file
 *
 * Revision 1.2  2015/09/09 20:29:39  akio
 * Adding Vetoing TriggerId
 * Also adding printing if option is set
 *
 * Revision 1.1  2008/01/23 04:45:07  kocolosk
 * Privileged Maker which skips events unless they fired any one of a set of supplied trigIDs
 *
 *****************************************************************************/
