// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.2 2008/08/02 04:18:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMP_HH
#define STBET4PMAKERIMP_HH

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

#include <utility>

namespace StSpinJet {
  class StJetTPC;
  class StJetBEMC;
  class StJetEEMC;
  class StJetTPCTrackCut;
  class StJetBEMCEnergyCut;

  class CorrectTowerEnergyForTracks;
}

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StSpinJet::StJetTPC* tpc,
		  StSpinJet::StJetTPCTrackCut* tpcCut,
		  StSpinJet::StJetBEMC* bemc,
		  StSpinJet::StJetBEMCEnergyCut* bemcCut,
		  StSpinJet::CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
		  StSpinJet::StJetEEMC* eemc);
    
  virtual ~StBET4pMakerImp() { };

  void Init();

  std::pair<StSpinJet::TrackList, StSpinJet::TowerEnergyList> getTrackAndEnergyList();

  StSpinJet::StJetTPC*  TPC()  { return _tpc;  }
  StSpinJet::StJetBEMC* BEMC() { return _bemc; }
  StSpinJet::StJetEEMC* EEMC() { return _eemc; }

  StSpinJet::StJetTPCTrackCut* getTPCTrackCut()     { return _tpcCut; }
  StSpinJet::StJetBEMCEnergyCut* getBEMCEnergyCut() { return _bemcCut; }

private:

  StSpinJet::StJetTPC*  _tpc;
  StSpinJet::StJetBEMC* _bemc;
  StSpinJet::StJetEEMC* _eemc;

  StSpinJet::StJetTPCTrackCut* _tpcCut;
  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_HH
