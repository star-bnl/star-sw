// -*- mode: c++;-*-
// $Id: StjMCParticleListReader.h,v 1.1 2008/08/22 17:25:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLELISTREADER_H
#define STJMCPARTICLELISTREADER_H

#include "StjTreeReader.h"

#include "StjMCParticleList.h"

#include <Rtypes.h>

class StjMCParticleListReader : public StjTreeReader {

public:
  StjMCParticleListReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjMCParticleListReader() { }

  StjMCParticleList getMCParticleList() { return _list; }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  StjMCParticleList _list;

  Int_t    _runNumber;
  Int_t    _eventId;
  Int_t    _nMCParticles;
  Int_t    _mcparticleId[4096];
  Int_t    _pdg[4096];
  Int_t    _firstMotherId[4096];
  Int_t    _lastMotherId[4096];
  Int_t    _firstDaughterId[4096];
  Int_t    _lastDaughterId[4096];
  Double_t _pt[4096];
  Double_t _eta[4096];
  Double_t _phi[4096];
  Double_t _m[4096];
  Double_t _e[4096];
  Int_t    _status[4096]; // 1: stable  2: unstable  3: incoming and parton
  Double_t _vertexZ;

  ClassDef(StjMCParticleListReader, 1)

};

#endif // STJMCPARTICLELISTREADER_H
