// -*- mode: c++;-*-
// $Id: StJetTrg.h,v 1.1 2008/07/23 02:34:03 tai Exp $
#ifndef STJETTRG_H
#define STJETTRG_H

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

#include <string>
#include <vector>
#include <map>
#include <algorithm>


class StJetTrgSoftware {

public:
  StJetTrgSoftware(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgSoftware() { }

  bool soft(int trgId)
  {
    return _emcTrigMaker->isTrigger(trgId);
  }

  std::vector<int> towers(int trgId)
  {
    std::vector<int> ret;
    std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(trgId);
    for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
      ret.push_back(tower->first);
    }
    std::sort(ret.begin(), ret.end());
    return ret;
  }

  std::vector<int> jetPatches(int trgId)
  {
    vector<int> ret;
    map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(trgId);
    for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
      ret.push_back(jp->first);
    }
    std::sort(ret.begin(), ret.end());
    return ret;
  }


private:

  StEmcTriggerMaker* _emcTrigMaker;

};

class StJetTrg {

public:
  StJetTrg(StMuDstMaker* uDstMaker, StJetTrgSoftware* soft)
    : _soft(soft)
    , _uDstMaker(uDstMaker)
  { }
  virtual ~StJetTrg() { }

  int runNumber()
  {
    return _uDstMaker->muDst()->event()->runId();
  }

  int eventId()
  {
    return _uDstMaker->muDst()->event()->eventId();
  }

  bool hard(int trgId)
  {
    return _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trgId);
  }

  bool soft(int trgId)
  {
    return _soft->soft(trgId);
  }

  double prescale(int trgId)
  {
    return StDetectorDbTriggerID::instance()->getTotalPrescales()[trgId];
  }

  double vertexZ()
  {
    return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
  }

  std::vector<int> towers(int trgId)
  {
    return _soft->towers(trgId);
  }

  std::vector<int> jetPatches(int trgId)
  {
    return _soft->jetPatches(trgId);
  }

private:

  StJetTrgSoftware* _soft;

  StMuDstMaker* _uDstMaker;

};


#endif // STJETTRG_H
