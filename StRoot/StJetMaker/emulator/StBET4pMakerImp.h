// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.4 2008/08/02 22:43:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMP_H
#define STBET4PMAKERIMP_H

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

#include <utility>

namespace StSpinJet {
  class StjTPC;
  class StjBEMC;
  class StjEEMC;
  class StjTrackListCut;
  class StjTowerEnergyListCut;

  class StjTowerEnergyCorrectionForTracks;
}

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StSpinJet::StjTPC* tpc,
		  StSpinJet::StjTrackListCut* tpcCut,
		  StSpinJet::StjBEMC* bemc,
		  StSpinJet::StjTowerEnergyListCut* bemcCut,
		  StSpinJet::StjTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks,
		  StSpinJet::StjEEMC* eemc);
    
  virtual ~StBET4pMakerImp() { };

  void Init();

  std::pair<StSpinJet::StjTrackList, StSpinJet::StjTowerEnergyList> getTrackAndEnergyList();

  StSpinJet::StjTPC*  TPC()  { return _tpc;  }
  StSpinJet::StjBEMC* BEMC() { return _bemc; }
  StSpinJet::StjEEMC* EEMC() { return _eemc; }

  StSpinJet::StjTrackListCut* getTPCTrackCut()     { return _tpcCut; }
  StSpinJet::StjTowerEnergyListCut* getBEMCEnergyCut() { return _bemcCut; }

private:

  StSpinJet::StjTPC*  _tpc;
  StSpinJet::StjBEMC* _bemc;
  StSpinJet::StjEEMC* _eemc;

  StSpinJet::StjTrackListCut* _tpcCut;
  StSpinJet::StjTowerEnergyListCut* _bemcCut;

  StSpinJet::StjTowerEnergyCorrectionForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_H
