// -*- mode:c++ -*-

#ifndef STAR_StTriggerSimuResult
#define STAR_StTriggerSimuResult

// $Id: StTriggerSimuResult.h,v 1.8 2016/03/18 22:49:38 zchang Exp $

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

#include <utility>
using std::make_pair;

#include <map>
using std::map;

#include "TObject.h"

#include "StVirtualTriggerSimu.h"

class L2pedResults2006;
class L2jetResults2006;
class L2gammaResult;
class L2Result;

/// simple struct to return HHTP tower - trigger patch pairs
class HttpResult {
public:
    int towerId;
    int towerAdc;
    int triggerPatchId;
    int triggerPatchAdc;
//    ClassDef(HttpResult,1)
};

enum L2ResultType {kPed, kJet, kGammaBemc, kGammaEemc, kUpsilon};

class StTriggerSimuResult : public TObject
{
public:
  StTriggerSimuResult();
  
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
  
  /// returns simple struct encapsulating (id,adc) of HT and TP pair
  HttpResult httpPair(short towerId) const;
  
  /// returns address of specific L2 result struct -- cast it yourself
  /**
     returns address of a specific L2 result structure.  These structures are
     documented at
     L2ped2006:    StDaqLib/TRG/L2pedResults2006.h
     L2jet2006:    StDaqLib/TRG/L2jetResults2006.h
     L2gamma2006:  StDaqLib/TRG/L2gammaResult2006.h
     along with examples of how to unpack them
  **/

  const unsigned int* l2Result(L2ResultType algo, int year=2006) const;
  const unsigned int* l2Result() const;
  
  void setTriggerId(unsigned int);
  void setBbcDecision(StTriggerSimuDecision);
  void setBemcDecision(StTriggerSimuDecision);
  void setEemcDecision(StTriggerSimuDecision);
  void setL2Decision(StTriggerSimuDecision);
  
  void addHighTower(int towerId, int dsmAdc);
  void addTriggerPatch(int patchId, int dsmAdc);
  void addJetPatch(int jetPatchId, int dsmAdc);
  
  // 2009
  void addBarrelJetPatchAdc(int jp, int adc);
  void addEndcapJetPatchAdc(int jp, int adc);
  void addOverlapJetPatchAdc(int jp, int adc);
  
  const map<int,int>& barrelJetPatches() const;
  const map<int,int>& endcapJetPatches() const;
  const map<int,int>& overlapJetPatches() const;
  
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

    // 2009
    map<int,int> mBarrelJetPatches;
    map<int,int> mEndcapJetPatches;
    map<int,int> mOverlapJetPatches;
    
    unsigned int mL2Result[64];
    //enum L2ResultOffset { kPed2006=0, kJet2006=14, kGammaBemc2006=6, kGammaEemc2006=8, kUpsilon2006=10 }; //!
    
    ClassDef(StTriggerSimuResult, 1)
};

inline unsigned int StTriggerSimuResult::triggerId() const { return mTriggerId; }
inline StTriggerSimuDecision StTriggerSimuResult::bbcDecision() const { return (StTriggerSimuDecision)mBbcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::bemcDecision() const { return (StTriggerSimuDecision)mBemcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::eemcDecision() const { return (StTriggerSimuDecision)mEemcDecision; }
inline StTriggerSimuDecision StTriggerSimuResult::l2Decision() const { return (StTriggerSimuDecision)mL2Decision; }

inline const vector<short>& StTriggerSimuResult::highTowerIds() const { return mHighTowerIds; }
inline const vector<short>& StTriggerSimuResult::triggerPatchIds() const { return mTriggerPatchIds; }
inline const vector<short>& StTriggerSimuResult::jetPatchIds() const { return mJetPatchIds; }

inline void StTriggerSimuResult::setTriggerId(unsigned int tid) { mTriggerId = tid; }
inline void StTriggerSimuResult::setBbcDecision(StTriggerSimuDecision d) { mBbcDecision = d; }
inline void StTriggerSimuResult::setBemcDecision(StTriggerSimuDecision d) { mBemcDecision = d; }
inline void StTriggerSimuResult::setEemcDecision(StTriggerSimuDecision d) { mEemcDecision = d; }
inline void StTriggerSimuResult::setL2Decision(StTriggerSimuDecision d) { mL2Decision = d; }

// 2009
inline void StTriggerSimuResult::addBarrelJetPatchAdc(int jp, int adc) { mBarrelJetPatches.insert(make_pair(jp,adc)); }
inline void StTriggerSimuResult::addEndcapJetPatchAdc(int jp, int adc) { mEndcapJetPatches.insert(make_pair(jp,adc)); }
inline void StTriggerSimuResult::addOverlapJetPatchAdc(int jp, int adc) { mOverlapJetPatches.insert(make_pair(jp,adc)); }

inline const map<int,int>& StTriggerSimuResult::barrelJetPatches() const { return mBarrelJetPatches; }
inline const map<int,int>& StTriggerSimuResult::endcapJetPatches() const { return mEndcapJetPatches; }
inline const map<int,int>& StTriggerSimuResult::overlapJetPatches() const { return mOverlapJetPatches; }

#endif

/*****************************************************************************
 * $Log: StTriggerSimuResult.h,v $
 * Revision 1.8  2016/03/18 22:49:38  zchang
 * updating trigger simulator for run12 analysis
 *
 * Revision 1.7  2010/08/13 00:21:27  rfatemi
 * changed PIG+2 address from BEMC to EEMC based on structure in StTriggerData2005::isL2Trigger()
 *
 * Revision 1.6  2010/04/29 10:34:34  pibero
 * Preserve backward compatibility with reading of Run 6 skim trees
 *
 * Revision 1.5  2010/02/18 20:07:03  pibero
 * Run 9 updates
 *
 * Revision 1.4  2008/01/22 18:06:27  kocolosk
 * added detailedResult code for BEMC L0, courtesy Dave Staszak
 * fixed two bugs in vector accessors in result class (also thanks to Dave)
 *
 * Revision 1.3  2008/01/18 02:10:30  kocolosk
 * add comment pointing to documentation of L2 result structures
 *
 * Revision 1.2  2008/01/17 17:04:08  kocolosk
 * some revisions to StTriggerSimuResult structure to hopefully improve clarity and maintainability
 *
 * Revision 1.1  2008/01/17 01:58:26  kocolosk
 * StTriggerSimuResult makes detailed emulation results persistent
 *
 *
 *****************************************************************************/
