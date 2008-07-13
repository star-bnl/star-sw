// $Id: StBET4pMakerImp.cxx,v 1.88 2008/07/13 10:02:31 tai Exp $
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

void StBET4pMakerImp::Make()
{
  TrackList trackList = _tpc->getTrackList();
  TowerEnergyList bemcEnergyList = _bemc->getEnergyList();
  TowerEnergyList eemcEnergyList = _eemc->getEnergyList();

  trackList = (*_tpcCut)(trackList);
  bemcEnergyList = _bemcCut->Apply(bemcEnergyList);

  TowerEnergyList bemcCorrectedEnergyList = _correctTowerEnergyForTracks->Do(bemcEnergyList, trackList);

  FourList tpc4pList = _track2four(trackList);
  _tracks.insert(_tracks.end(), tpc4pList.begin(), tpc4pList.end());

  FourList bemc4pList = _energy2four(bemcCorrectedEnergyList);
  _tracks.insert(_tracks.end(), bemc4pList.begin(), bemc4pList.end());

  FourList eemc4pList = _energy2four(eemcEnergyList);
  _tracks.insert(_tracks.end(), eemc4pList.begin(), eemc4pList.end());
}

