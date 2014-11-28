// $Id: StjMCParticleListWriter.cxx,v 1.4 2008/08/04 06:10:46 tai Exp $
#include "StjMCParticleListWriter.h"

#include <TDirectory.h>
#include <TTree.h>

#include <iostream>

ClassImp(StjMCParticleListWriter)

using namespace std;

StjMCParticleListWriter::StjMCParticleListWriter(const char* treeName, TDirectory* file)
  : _file(file)
{
  _file->cd();
  _tree = new TTree(treeName, treeName);
  _tree->SetAutoSave(kMaxLong64);
  _tree->SetMaxTreeSize(kMaxLong64);

  _tree->Branch("eventId"         , &_eventId         , "eventId/I"                       );
  _tree->Branch("nMCParticles"    , &_nMCParticles    , "nMCParticles/I"                  );
  _tree->Branch("mcparticleId"    ,  _mcparticleId    , "mcparticleId[nMCParticles]/I"    );    
  _tree->Branch("status"          ,  _status          , "status[nMCParticles]/I"          );    
  _tree->Branch("pdg"             ,  _pdg             , "pdg[nMCParticles]/I"             );    
  _tree->Branch("pt"              ,  _pt              , "pt[nMCParticles]/D"              );
  _tree->Branch("eta"             ,  _eta             , "eta[nMCParticles]/D"             );
  _tree->Branch("phi"             ,  _phi             , "phi[nMCParticles]/D"             );
  _tree->Branch("m"               ,  _m               , "m[nMCParticles]/D"               );
  _tree->Branch("e"               ,  _e               , "e[nMCParticles]/D"               );
  _tree->Branch("firstMotherId"   ,  _firstMotherId   , "firstMotherId[nMCParticles]/I"   );
  _tree->Branch("lastMotherId"    ,  _lastMotherId    , "lastMotherId[nMCParticles]/I"    );
  _tree->Branch("firstDaughterId" ,  _firstDaughterId , "firstDaughterId[nMCParticles]/I" );
  _tree->Branch("lastDaughterId"  ,  _lastDaughterId  , "lastDaughterId[nMCParticles]/I"  );
  _tree->Branch("vertexZ"         , &_vertexZ         , "vertexZ/D"                       );
  _tree->Branch("runNumber"       , &_runNumber       , "runNumber/I"                     );
}

void StjMCParticleListWriter::Fill(const StjMCParticleList& theList)
{
  if(theList.empty()) return;

  _runNumber =  theList[0].runNumber;
  _eventId   =  theList[0].eventId;
  _vertexZ   =  theList[0].vertexZ;

  _nMCParticles = theList.size();
  for(int i = 0; i < _nMCParticles; ++i) {
    const StjMCParticle& particle = theList[i];
    _mcparticleId[i]    = particle.mcparticleId;
    _pdg[i]             = particle.pdg;
    _firstMotherId[i]   = particle.firstMotherId;
    _lastMotherId[i]    = particle.lastMotherId;
    _firstDaughterId[i] = particle.firstDaughterId;
    _lastDaughterId[i]  = particle.lastDaughterId;
    _pt[i]              = particle.pt;
    _eta[i]             = particle.eta;
    _phi[i]             = particle.phi;
    _m[i]               = particle.m;
    _e[i]               = particle.e;
    _status[i]          = particle.status;
  }

  _tree->Fill();
}

void StjMCParticleListWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
