// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.10 2010/04/24 04:15:35 pibero Exp $
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

class StjAbstractTowerEnergyCorrectionForTracks;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StjTPC* tpc,
		  StjTrackListCut* tpcCut,
		  StjBEMC* bemc,
		  StjTowerEnergyListCut* bemcCut,
		  StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks,
		  StjEEMC* eemc);
    
  virtual ~StBET4pMakerImp() { };

  void Init();

  std::pair<StjTrackList, StjTowerEnergyList> getTrackAndEnergyList();

  StjTPC*  TPC()  { return _tpc;  }
  StjBEMC* BEMC() { return _bemc; }
  StjEEMC* EEMC() { return _eemc; }

  StjTrackListCut* getTPCTrackCut() { return _tpcCut; }
  StjTowerEnergyListCut* getBEMCEnergyCut() { return _bemcCut; }

private:

  StjTPC*  _tpc;
  StjBEMC* _bemc;
  StjEEMC* _eemc;

  StjTrackListCut* _tpcCut;
  StjTowerEnergyListCut* _bemcCut;

  StjAbstractTowerEnergyCorrectionForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_H
