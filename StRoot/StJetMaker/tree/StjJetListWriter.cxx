// $Id: StjJetListWriter.cxx,v 1.8 2009/12/03 09:57:36 pibero Exp $
#include "StjJetListWriter.h"

#include <TFile.h>
#include <TTree.h>

#include <iostream>

ClassImp(StjJetListWriter)

using namespace std;

StjJetListWriter::StjJetListWriter(const char* jetTreeName, const char* jetFourVecTreeName, TDirectory* file)
  : _file(file)
{
  _file->cd();

  _jetTree = new TTree(jetTreeName, jetTreeName);
  _jetTree->SetAutoSave(kMaxLong64);
  _jetTree->SetMaxTreeSize(kMaxLong64);

  _jetTree->Branch("eventId"    , &_jet_eventId      , "eventId/I"       );
  _jetTree->Branch("nJets"      , &_jet_nJets        , "nJets/I"         );
  _jetTree->Branch("jetId"      ,  _jet_jetId        , "jetId[nJets]/I"  );     
  _jetTree->Branch("pt"         ,  _jet_pt           , "pt[nJets]/D"     );
  _jetTree->Branch("eta"        ,  _jet_eta          , "eta[nJets]/D"    );
  _jetTree->Branch("detectorEta",  _jet_detectorEta  , "detectorEta[nJets]/D");    
  _jetTree->Branch("phi"        ,  _jet_phi          , "phi[nJets]/D"    );
  _jetTree->Branch("m"          ,  _jet_m            , "m[nJets]/D"      );
  _jetTree->Branch("neuRt"      ,  _jet_neuRt        , "neuRt[nJets]/D"  );
  _jetTree->Branch("vertexZ"    , &_jet_vertexZ      , "vertexZ/D");    
  _jetTree->Branch("runNumber"  , &_jet_runNumber    , "runNumber/I"     );

  _jetFourVecTree = new TTree(jetFourVecTreeName, jetFourVecTreeName);
  _jetFourVecTree->SetAutoSave(kMaxLong64);
  _jetFourVecTree->SetMaxTreeSize(kMaxLong64);

  _jetFourVecTree->Branch("eventId"     , &_four_eventId      , "eventId/I"                );
  _jetFourVecTree->Branch("nFourVecs"   , &_four_nFourVecs    , "nFourVecs/I"              );
  _jetFourVecTree->Branch("jetId"       ,  _four_jetId        , "jetId[nFourVecs]/I"       );     
  _jetFourVecTree->Branch("pt"          ,  _four_pt           , "pt[nFourVecs]/D"          );
  _jetFourVecTree->Branch("eta"         ,  _four_eta          , "eta[nFourVecs]/D"         );
  _jetFourVecTree->Branch("phi"         ,  _four_phi          , "phi[nFourVecs]/D"         );
  _jetFourVecTree->Branch("towerId"     ,  _four_towerId      , "towerId[nFourVecs]/I"     );     
  _jetFourVecTree->Branch("trackId"     ,  _four_trackId      , "trackId[nFourVecs]/S"     );     
  _jetFourVecTree->Branch("mcparticleId",  _four_mcparticleId , "mcparticleId[nFourVecs]/I");     
  _jetFourVecTree->Branch("m"           ,  _four_m            , "m[nFourVecs]/D"           );
  _jetFourVecTree->Branch("type"        ,  _four_type         , "type[nFourVecs]/I"        );     
  _jetFourVecTree->Branch("detectorId"  ,  _four_detectorId   , "detectorId[nFourVecs]/I"  );     
  _jetFourVecTree->Branch("fourvecId"   ,  _four_fourvecId    , "fourvecId[nFourVecs]/I"   );     
  _jetFourVecTree->Branch("runNumber"   , &_four_runNumber    , "runNumber/I"              );
  _jetFourVecTree->Branch("vertexZ"     , &_four_vertexZ      , "vertexZ/D"              );
}

void StjJetListWriter::Fill(const StjJetList& jetList)
{
  if(jetList.empty()) return;

  _jet_runNumber = jetList[0].runNumber;
  _jet_eventId   = jetList[0].eventId;
  _jet_vertexZ   = jetList[0].vertexZ;

  _four_nFourVecs = 0;

  _jet_nJets = jetList.size();
  for(int i = 0; i < _jet_nJets; ++i) {
    const StjJet& jet = jetList[i];
    _jet_jetId[i]       = jet.jetId;
    _jet_pt[i]          = jet.pt;
    _jet_eta[i]         = jet.eta;
    _jet_phi[i]         = jet.phi;
    _jet_m[i]           = jet.m;
    _jet_neuRt[i]       = jet.neuRt;
    _jet_detectorEta[i] = jet.detectorEta;
    fillFourVecTree(jet.jetId, jet.fourVecList);
  }

  _jetTree->Fill();
  _jetFourVecTree->Fill();
}

void StjJetListWriter::fillFourVecTree(int jetId, const StjFourVecList& fourVecList)
{
  if(fourVecList.empty()) return;

  _four_runNumber = fourVecList[0].runNumber;
  _four_eventId   = fourVecList[0].eventId;
  _four_vertexZ   = fourVecList[0].vertexZ;

  for(size_t i = 0; i < fourVecList.size(); ++i) {
    const StjFourVec& four = fourVecList[i];
    _four_fourvecId[_four_nFourVecs]    = four.fourvecId;
    _four_jetId[_four_nFourVecs]        = jetId;
    _four_type[_four_nFourVecs]         = four.type;
    _four_detectorId[_four_nFourVecs]   = four.detectorId;
    _four_trackId[_four_nFourVecs]      = four.trackId;
    _four_towerId[_four_nFourVecs]      = four.towerId;
    _four_mcparticleId[_four_nFourVecs] = four.mcparticleId;
    _four_pt[_four_nFourVecs]           = four.pt;
    _four_eta[_four_nFourVecs]          = four.eta;
    _four_phi[_four_nFourVecs]          = four.phi;
    _four_m[_four_nFourVecs]            = four.m;
    _four_nFourVecs++;
  }
}

void StjJetListWriter::Finish()
{
  _jetTree->BuildIndex("runNumber", "eventId");
  _jetFourVecTree->BuildIndex("runNumber", "eventId");
}
