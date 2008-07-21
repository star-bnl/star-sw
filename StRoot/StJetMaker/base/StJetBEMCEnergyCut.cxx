// $Id: StJetBEMCEnergyCut.cxx,v 1.1 2008/07/21 17:24:44 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetBEMCEnergyCut.h"

using namespace std;

namespace StSpinJet {


StSpinJet::TowerEnergyList StJetBEMCEnergyCut::operator()(const TowerEnergyList &energyList)
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
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(energyDeposit)) return true;
  }

  return false;
}


}
