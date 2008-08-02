// $Id: StBET4pMakerImp.cxx,v 1.2 2008/08/02 04:18:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StBET4pMakerImp.h"

#include "StjTPC.h"
#include "StjBEMC.h"
#include "StjEEMC.h"

#include "StjTrackListCut.h"
#include "StjTowerEnergyListCut.h"

#include "StjTowerEnergyCorrectionForTracks.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;


StBET4pMakerImp::StBET4pMakerImp(StJetTPC* tpc, StJetTPCTrackCut* tpcCut,
				 StJetBEMC* bemc, StJetBEMCEnergyCut* bemcCut,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
				 StJetEEMC* eemc)
  : _tpc(tpc), _bemc(bemc), _eemc(eemc)
  , _tpcCut(tpcCut), _bemcCut(bemcCut)
  , _correctTowerEnergyForTracks(correctTowerEnergyForTracks)
{

}

void StBET4pMakerImp::Init()
{
  _tpc->Init();
  _bemc->Init();
  _eemc->Init();
}

std::pair<TrackList, TowerEnergyList> StBET4pMakerImp::getTrackAndEnergyList()
{
  TrackList trackList = _tpc->getTrackList();
  TowerEnergyList bemcEnergyList = _bemc->getEnergyList();
  TowerEnergyList eemcEnergyList = _eemc->getEnergyList();

  trackList = (*_tpcCut)(trackList);
  bemcEnergyList = (*_bemcCut)(bemcEnergyList);

  bemcEnergyList = (*_correctTowerEnergyForTracks)(bemcEnergyList, trackList);

  TowerEnergyList energyList;
  copy(bemcEnergyList.begin(), bemcEnergyList.end(), back_inserter(energyList));
  copy(eemcEnergyList.begin(), eemcEnergyList.end(), back_inserter(energyList));

  return pair<TrackList, TowerEnergyList>(trackList, energyList);
}
