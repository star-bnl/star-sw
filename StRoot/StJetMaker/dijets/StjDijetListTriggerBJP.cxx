// $Id: StjDijetListTriggerBJP.cxx,v 1.2 2008/09/12 22:32:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjDijetListTriggerBJP.h"

#include <StjTrg.h>
#include <StjTrgBEMCJetPatchTowerIdMap.h>

#include <vector>

ClassImp(StjDijetListTriggerBJP)

using namespace std;

StjDijetList StjDijetListTriggerBJP::processOneItem(const StjDijetList::value_type& item)
{
  StjDijetList retList;

  if( ! _trg->passed() ) return retList;

  double maxEt3 = getMaxEtForBEMCJetPatchThatPassedBJP(item.jet3);
  double maxEt4 = getMaxEtForBEMCJetPatchThatPassedBJP(item.jet4);

  if(maxEt3 == 0 && maxEt4 == 0) return retList;

  StjDijet dijet(item);

  if(maxEt3 >= maxEt4)
    {
      dijet.jetSameSide = item.jet3;
      dijet.jetAwaySide = item.jet4;
      dijet.neuRtSameSide = item.jet3.neuRt;
      dijet.neuRtAwaySide = item.jet4.neuRt;
    }
  else
    {
      dijet.jetSameSide = item.jet4;
      dijet.jetAwaySide = item.jet3;
      dijet.neuRtSameSide = item.jet4.neuRt;
      dijet.neuRtAwaySide = item.jet3.neuRt;
    }

  retList.push_back(dijet);

  return retList;
}

double StjDijetListTriggerBJP::getMaxEtForBEMCJetPatchThatPassedBJP(const StjJet& jet)
{
  double ret = 0;
  for(StjFourVecList::const_iterator it = jet.fourVecList.begin();  it != jet.fourVecList.end(); ++it)
    {
      double jetPatchEt = getJetPatchEtIfFiredBJP(*it);
      if(jetPatchEt > ret) ret = jetPatchEt;
    }
  return ret;
}

double StjDijetListTriggerBJP::getJetPatchEtIfFiredBJP(const StjFourVec& p4)
{
  if(p4.type != 2) return 0;
  if(p4.detectorId != 9) return 0;
  int jp = _jetPatchTowerMap->getJetPatchIdForTower(p4.towerId);
  vector<int> jetPatches = _trg->jetPatches();
  vector<double> jetPatchEt = _trg->jetPatchEt();
  for(size_t i = 0; i != jetPatches.size(); ++i) 
    {
      if(jp == jetPatches[i]) return jetPatchEt[i];
    }
  return 0;
}

