// $Id: StBET4pMakerImp.cxx,v 1.82 2008/07/10 10:09:03 tai Exp $

#include "StBET4pMakerImp.h"

#include "StJetTPC.h"
#include "StJetBEMC.h"
#include "StJetEEMC.h"

#include "StJetTPCTrackCut.h"
#include "StJetBEMCEnergyCut.h"

#include "CorrectTowerEnergyForTracks.h"

#include "TrackListToFourList.h"
#include "EnergyListToFourList.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;


StBET4pMakerImp::StBET4pMakerImp(StJetTPC* tpc, StJetTPCTrackCut* tpcCut,
				 StJetBEMC* bemc, StJetBEMCEnergyCut* bemcCut,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
				 StJetEEMC* eemc)
  : mUseEndcap(false)
  , mUseBEMC(true)
  , mUseTPC(true)
  , _tpc(tpc), _bemc(bemc), _eemc(eemc)
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
  if(mUseEndcap) _eemc->Init();
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
  TrackList trackList;

  if(mUseTPC) {
    trackList = _tpc->getTrackList();

    trackList = (*_tpcCut)(trackList);

    FourList tpc4pList = _track2four(trackList);

    _tracks.insert(_tracks.end(), tpc4pList.begin(), tpc4pList.end());
  }


  if(mUseBEMC) {
    TowerEnergyDepositList bemcEnergyList = _bemc->getEnergyList();

    bemcEnergyList = _bemcCut->Apply(bemcEnergyList);

    TowerEnergyDepositList bemcCorrectedEnergyDepositList = _correctTowerEnergyForTracks->Do(bemcEnergyList, trackList);

    FourList bemc4pList = _energy2four(bemcCorrectedEnergyDepositList);

    _tracks.insert(_tracks.end(), bemc4pList.begin(), bemc4pList.end());
  }


  if(mUseEndcap) {

    TowerEnergyDepositList eemcEnergyList = _eemc->getEnergyList();

    FourList eemc4pList = _energy2four(eemcEnergyList);

    _tracks.insert(_tracks.end(), eemc4pList.begin(), eemc4pList.end());
  }

}



