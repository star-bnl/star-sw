// $Id: StjMCParticleListReader.cxx,v 1.2 2008/08/22 18:36:32 tai Exp $
#include "StjMCParticleListReader.h"

#include <TTree.h>

#include <iostream>

ClassImp(StjMCParticleListReader)

using namespace std;

void StjMCParticleListReader::SetBranchAddress(TTree *tree)
{
  tree->SetBranchAddress("eventId"         , &_eventId         );
  tree->SetBranchAddress("nMCParticles"    , &_nMCParticles    );
  tree->SetBranchAddress("mcparticleId"    ,  _mcparticleId    );    
  tree->SetBranchAddress("status"          ,  _status          );    
  tree->SetBranchAddress("pdg"             ,  _pdg             );    
  tree->SetBranchAddress("pt"              ,  _pt              );
  tree->SetBranchAddress("eta"             ,  _eta             );
  tree->SetBranchAddress("phi"             ,  _phi             );
  tree->SetBranchAddress("m"               ,  _m               );
  tree->SetBranchAddress("e"               ,  _e               );
  tree->SetBranchAddress("firstMotherId"   ,  _firstMotherId   );
  tree->SetBranchAddress("lastMotherId"    ,  _lastMotherId    );
  tree->SetBranchAddress("firstDaughterId" ,  _firstDaughterId );
  tree->SetBranchAddress("lastDaughterId"  ,  _lastDaughterId  );
  tree->SetBranchAddress("vertexZ"         , &_vertexZ         );
  tree->SetBranchAddress("runNumber"       , &_runNumber       );
}

void StjMCParticleListReader::clearEntry()
{
  _list.clear();
}

void StjMCParticleListReader::readEntry()
{
  clearEntry();

  for(int i = 0; i < _nMCParticles; ++i) {

    StjMCParticle mcparticle;

    mcparticle.runNumber       = _runNumber;
    mcparticle.eventId         = _eventId;
    mcparticle.mcparticleId    = _mcparticleId[i];
    mcparticle.pdg             = _pdg[i];
    mcparticle.firstMotherId   = _firstMotherId[i];
    mcparticle.lastMotherId    = _lastMotherId[i];
    mcparticle.firstDaughterId = _firstDaughterId[i];
    mcparticle.lastDaughterId  = _lastDaughterId[i];
    mcparticle.pt              = _pt[i];
    mcparticle.eta             = _eta[i];
    mcparticle.phi             = _phi[i];
    mcparticle.m               = _m[i];
    mcparticle.e               = _e[i];
    mcparticle.status          = _status[i];
    mcparticle.vertexZ         = _vertexZ;

    _list.push_back(mcparticle);
  }
}
