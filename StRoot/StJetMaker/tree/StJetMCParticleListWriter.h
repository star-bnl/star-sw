// -*- mode: c++;-*-
// $Id: StJetMCParticleListWriter.h,v 1.1 2008/07/25 01:06:01 tai Exp $
#ifndef STJETMCPARTICLELISTWRITER_HH
#define STJETMCPARTICLELISTWRITER_HH

#include "MCParticleList.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetMCParticleListWriter {

public:

  StJetMCParticleListWriter(const char* treeName, TDirectory* file);
  virtual ~StJetMCParticleListWriter() { }

  void Fill(const StSpinJet::MCParticleList& theList);
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
