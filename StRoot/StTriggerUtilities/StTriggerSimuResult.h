#ifndef STAR_StTriggerSimuResult
#define STAR_StTriggerSimuResult

// $Id: StTriggerSimuResult.h,v 1.1 2008/01/17 01:58:26 kocolosk Exp $

/*****************************************************************************
 * @class StTriggerSimuResult
 * @author A.Kocoloski
 *
 * Stores detailed simulator information for a single trigger ID.
 *****************************************************************************/

#include <utility>
using std::pair;

#include <vector>
using std::vector;

#include "TObject.h"

#include "StVirtualTriggerSimu.h"

class L2pedResults2006;
class L2jetResults2006;
class L2gammaResult;
class L2Result;

class StTriggerSimuResult : public TObject
{
public:
    StTriggerSimuResult();
    StTriggerSimuResult(const StTriggerSimuResult&);
    virtual ~StTriggerSimuResult();
    
    unsigned int triggerId() const;
    StTriggerSimuDecision bbcDecision() const;
    StTriggerSimuDecision bemcDecision() const;
    StTriggerSimuDecision eemcDecision() const;
    StTriggerSimuDecision l2Decision() const;
    
    const vector<short>& highTowerIds() const;
    const vector<short>& triggerPatchIds() const;
    const vector<short>& jetPatchIds() const;
    
    /// returns DSM ADC if above trigger threshold, otherwise -1
    int highTowerAdc(short towerId) const;
    
    /// returns DSM ADC if above trigger threshold, otherwise -1    
    int triggerPatchAdc(short patchId) const;
    
    /// returns DSM ADC if above trigger threshold, otherwise -1
    int jetPatchAdc(short jetPatchId) const;
    
    /// returns DSM ADC for <HT, TP> -- check that both are > 0
    pair<int, int> httpAdcPair(short towerId) const;
    
    const L2pedResults2006& l2PedResult2006() const;
    const L2jetResults2006& l2JetResult2006() const;
    const L2gammaResult& l2GammaBemcResult2006() const;
    const L2gammaResult& l2GammaEemcResult2006() const;
    const L2Result& l2UpsilonResult2006() const;
    
    void setTriggerId(unsigned int);
    void setBbcDecision(StTriggerSimuDecision);
    void setBemcDecision(StTriggerSimuDecision);
    void setEemcDecision(StTriggerSimuDecision);
    void setL2Decision(StTriggerSimuDecision);
    
    void addHighTower(int towerId, int dsmAdc);
    void addTriggerPatch(int patchId, int dsmAdc);
    void addJetPatch(int jetPatchId, int dsmAdc);
    
    void setL2Result(const unsigned int* result);
    
private:
    UInt_t mTriggerId;
    UChar_t mBbcDecision;
    UChar_t mBemcDecision;
    UChar_t mEemcDecision;
    UChar_t mL2Decision;
    
    vector<short> mHighTowerIds;
    vector<short> mHighTowerAdcs;
    
    vector<short> mTriggerPatchIds;
    vector<short> mTriggerPatchAdcs;
    
    vector<short> mJetPatchIds;
    vector<short> mJetPatchAdcs;
    
    UInt_t mL2Result[32];
    //enum L2ResultOffset { kPed2006=0, kJet2006=14, kGammaBemc2006=6, kGammaEemc2006=8, kUpsilon2006=10 }; //!
    
    ClassDef(StTriggerSimuResult, 1)
};

inline unsigned int StTriggerSimuResult::triggerId() const { return mTriggerId; }
inline StTriggerSimuDecision StTriggerSimuResult::bbcDecision() const { return (StTriggerSimuDecision)mBbcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::bemcDecision() const { return (StTriggerSimuDecision)mBemcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::eemcDecision() const { return (StTriggerSimuDecision)mEemcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::l2Decision() const { return (StTriggerSimuDecision)mL2Decision; }

inline const vector<short>& StTriggerSimuResult::highTowerIds() const { return mHighTowerIds; }
inline const vector<short>& StTriggerSimuResult::triggerPatchIds() const { return mTriggerPatchAdcs; }
inline const vector<short>& StTriggerSimuResult::jetPatchIds() const { return mJetPatchAdcs; }

inline void StTriggerSimuResult::setTriggerId(unsigned int tid) { mTriggerId = tid; }
inline void StTriggerSimuResult::setBbcDecision(StTriggerSimuDecision d) { mBbcDecision = d; }
inline void StTriggerSimuResult::setBemcDecision(StTriggerSimuDecision d) { mBemcDecision = d; }
inline void StTriggerSimuResult::setEemcDecision(StTriggerSimuDecision d) { mEemcDecision = d; }
inline void StTriggerSimuResult::setL2Decision(StTriggerSimuDecision d) { mL2Decision = d; }
#endif

/*****************************************************************************
 * $Log: StTriggerSimuResult.h,v $
 * Revision 1.1  2008/01/17 01:58:26  kocolosk
 * StTriggerSimuResult makes detailed emulation results persistent
 *
 *
 *****************************************************************************/
