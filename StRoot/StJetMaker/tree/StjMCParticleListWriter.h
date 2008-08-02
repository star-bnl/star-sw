// -*- mode: c++;-*-
// $Id: StjMCParticleListWriter.h,v 1.2 2008/08/02 19:23:38 tai Exp $
#ifndef STJETMCPARTICLELISTWRITER_HH
#define STJETMCPARTICLELISTWRITER_HH

#include "StjMCParticleList.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StjMCParticleListWriter {

public:

  StjMCParticleListWriter(const char* treeName, TDirectory* file);
  virtual ~StjMCParticleListWriter() { }

  void Fill(const StSpinJet::StjMCParticleList& theList);
  void Finish();
    
private:

  TDirectory* _file;
  TTree* _tree;

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
};

#endif // STJETMCPARTICLELISTWRITER_HH
