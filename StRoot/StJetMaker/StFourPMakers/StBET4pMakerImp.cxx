// $Id: StBET4pMakerImp.cxx,v 1.81 2008/07/10 09:47:02 tai Exp $

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
  TrackList trackmuList;

  if(mUseTPC) {
    trackmuList = _tpc->getTrackList();

    trackmuList = (*_tpcCut)(trackmuList);

    FourList tpcFourMomentumList = _track2four(trackmuList);

    _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());
  }


  if(mUseBEMC) {
    TowerEnergyDepositList bemcEnergyDepositList = _bemc->getEnergyList();

    bemcEnergyDepositList = _bemcCut->Apply(bemcEnergyDepositList);

    TowerEnergyDepositList bemcCorrectedEnergyDepositList = _correctTowerEnergyForTracks->Do(bemcEnergyDepositList, trackmuList);

    FourList bemcFourMomentumList = _energy2four(bemcCorrectedEnergyDepositList);

    _tracks.insert(_tracks.end(), bemcFourMomentumList.begin(), bemcFourMomentumList.end());
  }


  if(mUseEndcap) {

    TowerEnergyDepositList eemcEnergyList = _eemc->getEnergyList();

    FourList eemcFourMomentumList = _energy2four(eemcEnergyList);

    _tracks.insert(_tracks.end(), eemcFourMomentumList.begin(), eemcFourMomentumList.end());
  }

}



