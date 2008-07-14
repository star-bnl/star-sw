// $Id: StBET4pMakerImp.cxx,v 1.89 2008/07/14 20:47:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StBET4pMakerImp.h"

#include "StJetTPC.h"
#include "StJetBEMC.h"
#include "StJetEEMC.h"

#include "StJetTPCTrackCut.h"
#include "StJetBEMCEnergyCut.h"

#include "CorrectTowerEnergyForTracks.h"

#include "TrackListToFourList.h"
#include "EnergyListToFourList.h"

#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

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
  , _track2four(*(new TrackListToFourList))
  , _energy2four(*(new EnergyListToFourList))
{

}

void StBET4pMakerImp::Init()
{
  _tpc->Init();
  _bemc->Init();
  _eemc->Init();
}

void StBET4pMakerImp::Clear(Option_t* opt)
{
    
  for (FourList::iterator it = _tracks.begin(); it != _tracks.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  _tracks.clear();

}

std::pair<TrackList, TowerEnergyList> StBET4pMakerImp::getTrackAndEnergyList()
{
  TrackList trackList = _tpc->getTrackList();
  TowerEnergyList bemcEnergyList = _bemc->getEnergyList();
  TowerEnergyList eemcEnergyList = _eemc->getEnergyList();

  trackList = (*_tpcCut)(trackList);
  bemcEnergyList = _bemcCut->Apply(bemcEnergyList);

  bemcEnergyList = _correctTowerEnergyForTracks->Do(bemcEnergyList, trackList);

  TowerEnergyList energyList;
  copy(bemcEnergyList.begin(), bemcEnergyList.end(), back_inserter(energyList));
  copy(eemcEnergyList.begin(), eemcEnergyList.end(), back_inserter(energyList));

  return pair<TrackList, TowerEnergyList>(trackList, energyList);
}

void StBET4pMakerImp::Make()
{
  pair<TrackList, TowerEnergyList> trackAndEnergyList = getTrackAndEnergyList();

  Clear(0);

  FourList tpc4pList = _track2four(trackAndEnergyList.first);
  _tracks.insert(_tracks.end(), tpc4pList.begin(), tpc4pList.end());

  FourList energy4pList = _energy2four(trackAndEnergyList.second);
  _tracks.insert(_tracks.end(), energy4pList.begin(), energy4pList.end());
}

