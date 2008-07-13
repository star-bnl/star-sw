// $Id: StJetBEMCEnergyCut.cxx,v 1.3 2008/07/13 01:43:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetBEMCEnergyCut.h"

#include <iostream>
#include <vector>

using namespace std;
using namespace StJetTowerEnergyCut;

namespace StSpinJet {


StSpinJet::TowerEnergyList StJetBEMCEnergyCut::Apply(const TowerEnergyList &energyList)
{
  TowerEnergyList ret;

  for(TowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);

  }

  return ret;
}


bool StJetBEMCEnergyCut::shouldNotKeep(const TowerEnergy& energyDeposit)
{
  TowerEnergyCut* cut1 = new TowerEnergyCut2003BemcTower();
  TowerEnergyCut* cut2 = new TowerEnergyCutBemcWestOnly();
  TowerEnergyCut* cut3 = new TowerEnergyCutEnergy();
  TowerEnergyCut* cut4 = new TowerEnergyCutBemcStatus();
  TowerEnergyCut* cut5 = new TowerEnergyCutAdc();

  if(mUse2003Cuts) _cutList.push_back(cut1);
  if(mUse2005Cuts) _cutList.push_back(cut2);
  _cutList.push_back(cut3);
  _cutList.push_back(cut4);
  _cutList.push_back(cut5);

  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(energyDeposit)) return true;
  }

  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    delete *cut;
  }
  
  _cutList.clear();

  return false;

}


}
