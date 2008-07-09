// $Id: StBET4pMakerImp.cxx,v 1.76 2008/07/09 10:58:09 tai Exp $

#include "StBET4pMakerImp.h"

#include "CollectEnergyDepositsFromBEMC.h"
#include "StJetEEMC.h"
#include "TrackListToFourList.h"
#include "EnergyListToFourList.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

StBET4pMakerImp::StBET4pMakerImp(CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
				 CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
				 StJetEEMC* eemc)
  : mUseEndcap(false)
  , mUseBEMC(true)
  , _collectChargedTracksFromTPC(collectChargedTracksFromTPC)
  , _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
  , _eemc(eemc)
  , _correctTowerEnergyForTracks(correctTowerEnergyForTracks)
  , _track2four(*(new TrackListToFourList))
  , _energy2four(*(new EnergyListToFourList))
{

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
  TrackList trackmuList = _collectChargedTracksFromTPC->Do();

  FourList tpcFourMomentumList = _track2four(trackmuList);

  _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());

  if(mUseBEMC) {
    TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

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



