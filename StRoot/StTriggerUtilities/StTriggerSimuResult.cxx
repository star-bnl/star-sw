// $Id: StTriggerSimuResult.cxx,v 1.6 2010/04/29 10:34:34 pibero Exp $

#include <utility>
using std::make_pair;

#include "StMessMgr.h"
#include "StTriggerSimuResult.h"

#include "StDaqLib/TRG/trgStructures2005.h"

#include "L2Emulator/L2pedAlgo/L2pedResults2006.h"
#include "L2Emulator/L2jetAlgo/L2jetResults2006.h"
#include "L2Emulator/L2gammaAlgo/L2gammaResult2006.h"
#include "L2Emulator/L2upsilon/L2upsilonResult2006.h"
#include <cstring>

ClassImp(HttpResult)
ClassImp(StTriggerSimuResult)

StTriggerSimuResult::StTriggerSimuResult() : TObject(), mBbcDecision(kDoNotCare),
    mBemcDecision(kDoNotCare), mEemcDecision(kDoNotCare), mL2Decision(kDoNotCare) { }

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

HttpResult StTriggerSimuResult::httpPair(short towerId) const {
    HttpResult result;
    result.towerId = -1;
    result.towerAdc = -1;
    result.triggerPatchId = -1;
    result.triggerPatchAdc = -1;
    for(unsigned i=0; i<mHighTowerIds.size(); i++) {
        if(mHighTowerIds[i] == towerId) {
            result.towerId = towerId;
            result.towerAdc = mHighTowerAdcs[i];
            if(i<mTriggerPatchAdcs.size()) {
                result.triggerPatchId = mTriggerPatchIds[i];
                result.triggerPatchAdc = mTriggerPatchAdcs[i];                
            }
            else {
                LOG_WARN << "No matching TP is available for " << towerId << endm;
            }
        }
    }
    return result;
}

const unsigned int* StTriggerSimuResult::l2Result(L2ResultType algo, int year) const {
    switch(algo) {
        case kPed:
            if(year==2006) return  mL2Result + L2RESULTS_OFFSET_EMC_PED;
            break;
        case kJet:
            if(year==2006) return mL2Result + L2RESULTS_OFFSET_DIJET;
            break;
        case kGammaBemc:
            if(year==2006) return mL2Result + L2RESULTS_OFFSET_PIG + 2;
            break;
        case kGammaEemc:
            if(year==2006) return mL2Result + L2RESULTS_OFFSET_PIG;
            break;
        case kUpsilon:
            if(year==2006) return mL2Result + L2RESULTS_OFFSET_UPS;
            break;
    }
    return 0;
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
    memcpy(mL2Result, result, sizeof(mL2Result));
}


/*****************************************************************************
 * $Log: StTriggerSimuResult.cxx,v $
 * Revision 1.6  2010/04/29 10:34:34  pibero
 * Preserve backward compatibility with reading of Run 6 skim trees
 *
 * Revision 1.5  2010/02/18 20:07:03  pibero
 * Run 9 updates
 *
 * Revision 1.4  2009/09/23 23:22:42  fine
 * add the missed cstring header file
 *
 * Revision 1.3  2008/01/30 15:09:24  balewski
 * Added upsilon2006, needs cleaunp in few days
 *
 * Revision 1.2  2008/01/17 17:04:08  kocolosk
 * some revisions to StTriggerSimuResult structure to hopefully improve clarity and maintainability
 *
 * Revision 1.1  2008/01/17 01:58:25  kocolosk
 * StTriggerSimuResult makes detailed emulation results persistent
 *
 *
 *****************************************************************************/
