// $Id: StjDijetListTriggerBHT.cxx,v 1.2 2008/09/12 22:32:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjDijetListTriggerBHT.h"

#include <StjTrg.h>

#include <vector>

ClassImp(StjDijetListTriggerBHT)

using namespace std;

StjDijetList StjDijetListTriggerBHT::processOneItem(const StjDijetList::value_type& item)
{
  StjDijetList retList;

  if( ! _trg->passed() ) return retList;

  double maxEt3 = getMaxEtForBEMCTowersThatPassedBHT(item.jet3);
  double maxEt4 = getMaxEtForBEMCTowersThatPassedBHT(item.jet4);

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

double StjDijetListTriggerBHT::getMaxEtForBEMCTowersThatPassedBHT(const StjJet& jet)
{
  double ret = 0;
  for(StjFourVecList::const_iterator it = jet.fourVecList.begin();  it != jet.fourVecList.end(); ++it)
    {
      double towerEt = getTowerEtIfFiredBHT(*it);
      if(towerEt > ret) ret = towerEt;
    }
  return ret;
}

double StjDijetListTriggerBHT::getTowerEtIfFiredBHT(const StjFourVec& p4)
{
  if(p4.type != 2) return 0;
  if(p4.detectorId != 9) return 0;
  vector<int> towers = _trg->towers();
  vector<double> towerEt = _trg->towerEt();
  for(size_t i = 0; i != towers.size(); ++i) 
    {
      if(p4.towerId == towers[i]) return towerEt[i];
    }
  return 0;
}

