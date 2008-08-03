// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.5 2008/08/03 00:26:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMP_H
#define STBET4PMAKERIMP_H

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

#include <utility>

class StjTPC;
class StjBEMC;
class StjEEMC;
class StjTrackListCut;
class StjTowerEnergyListCut;

class StjTowerEnergyCorrectionForTracks;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StjTPC* tpc,
		  StjTrackListCut* tpcCut,
		  StjBEMC* bemc,
		  StjTowerEnergyListCut* bemcCut,
		  StjTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks,
		  StjEEMC* eemc);
    
  virtual ~StBET4pMakerImp() { };

  void Init();

  std::pair<StjTrackList, StjTowerEnergyList> getTrackAndEnergyList();

  StjTPC*  TPC()  { return _tpc;  }
  StjBEMC* BEMC() { return _bemc; }
  StjEEMC* EEMC() { return _eemc; }

  StjTrackListCut* getTPCTrackCut()     { return _tpcCut; }
  StjTowerEnergyListCut* getBEMCEnergyCut() { return _bemcCut; }

private:

  StjTPC*  _tpc;
  StjBEMC* _bemc;
  StjEEMC* _eemc;

  StjTrackListCut* _tpcCut;
  StjTowerEnergyListCut* _bemcCut;

  StjTowerEnergyCorrectionForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_H
