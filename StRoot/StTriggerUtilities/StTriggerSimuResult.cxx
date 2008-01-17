// $Id: StTriggerSimuResult.cxx,v 1.1 2008/01/17 01:58:25 kocolosk Exp $

#include <utility>
using std::make_pair;

#include "StMessMgr.h"
#include "StTriggerSimuResult.h"

#include "StDaqLib/TRG/trgStructures2005.h"

#include "L2Emulator/L2pedAlgo/L2pedResults2006.h"
#include "L2Emulator/L2jetAlgo/L2jetResults2006.h"
#include "L2Emulator/L2gammaAlgo/L2gammaResult2006.h"
#include "L2Emulator/L2upsilon/L2Result.h"

ClassImp(StTriggerSimuResult)

StTriggerSimuResult::StTriggerSimuResult() : TObject(), mBbcDecision(kDoNotCare),
    mBemcDecision(kDoNotCare), mEemcDecision(kDoNotCare), mL2Decision(kDoNotCare) { }

StTriggerSimuResult::StTriggerSimuResult(const StTriggerSimuResult & o) {
    mTriggerId          = o.mTriggerId;
    mBbcDecision        = o.mBbcDecision;
    mBemcDecision       = o.mBemcDecision;
    mEemcDecision       = o.mEemcDecision;
    mL2Decision         = o.mL2Decision;
    mHighTowerIds       = o.mHighTowerIds;
    mHighTowerAdcs      = o.mHighTowerAdcs;
    mTriggerPatchIds    = o.mTriggerPatchIds;
    mTriggerPatchAdcs   = o.mTriggerPatchAdcs;
    mJetPatchIds        = o.mJetPatchIds;
    mJetPatchAdcs       = o.mJetPatchAdcs;
    memcpy( mL2Result, o.mL2Result, 128);
}

StTriggerSimuResult::~StTriggerSimuResult() { }

int StTriggerSimuResult::highTowerAdc(short towerId) const {
    for(unsigned i=0; i<mHighTowerIds.size(); i++) {
        if(mHighTowerIds[i] == towerId) return mHighTowerAdcs[i];
    }
    return -1;
}

int StTriggerSimuResult::triggerPatchAdc(short patchId) const {
    for(unsigned i=0; i<mTriggerPatchIds.size(); i++) {
        if(mTriggerPatchIds[i] == patchId) return mTriggerPatchAdcs[i];
    }
    return -1;
}


int StTriggerSimuResult::jetPatchAdc(short jetPatchId) const {
    for(unsigned i=0; i<mJetPatchIds.size(); i++) {
        if(mJetPatchIds[i] == jetPatchId) return mJetPatchAdcs[i];
    }
    return -1;
}

pair<int, int> StTriggerSimuResult::httpAdcPair(short towerId) const {
    for(unsigned i=0; i<mHighTowerIds.size(); i++) {
        if(mHighTowerIds[i] == towerId) {
            if(i<mTriggerPatchAdcs.size()) {
                return make_pair( mHighTowerAdcs[i], mTriggerPatchAdcs[i] );                
            }
            else {
                LOG_WARN << "No matching TP is available for " << towerId << endm;
                return make_pair( mHighTowerAdcs[i], -1 );                                
            }
        }
    }
    return make_pair(-1,-1);
}

const L2pedResults2006& StTriggerSimuResult::l2PedResult2006() const {
    return *(L2pedResults2006*)(mL2Result + L2RESULTS_OFFSET_EMC_PED);
}

const L2jetResults2006& StTriggerSimuResult::l2JetResult2006() const {
    return *(L2jetResults2006*)(mL2Result + L2RESULTS_OFFSET_DIJET);
}

const L2gammaResult& StTriggerSimuResult::l2GammaBemcResult2006() const {
    return *(L2gammaResult*)(mL2Result + L2RESULTS_OFFSET_PIG + 3);
}

const L2gammaResult& StTriggerSimuResult::l2GammaEemcResult2006() const {
    return *(L2gammaResult*)(mL2Result + L2RESULTS_OFFSET_PIG + 5);
}

const L2Result& StTriggerSimuResult::l2UpsilonResult2006() const {
    return *(L2Result*)(mL2Result + L2RESULTS_OFFSET_UPS);
}

void StTriggerSimuResult::addHighTower(int towerId, int dsmAdc) {
    mHighTowerIds.push_back(towerId);
    mHighTowerAdcs.push_back(dsmAdc);
}

void StTriggerSimuResult::addTriggerPatch(int patchId, int dsmAdc) {
    mTriggerPatchIds.push_back(patchId);
    mTriggerPatchAdcs.push_back(dsmAdc);
}

void StTriggerSimuResult::addJetPatch(int jetPatchId, int dsmAdc) {
    mJetPatchIds.push_back(jetPatchId);
    mJetPatchAdcs.push_back(dsmAdc);
}

void StTriggerSimuResult::setL2Result(const unsigned int* result) {
    memcpy(mL2Result, result, 128);
}


/*****************************************************************************
 * $Log: StTriggerSimuResult.cxx,v $
 * Revision 1.1  2008/01/17 01:58:25  kocolosk
 * StTriggerSimuResult makes detailed emulation results persistent
 *
 *
 *****************************************************************************/
