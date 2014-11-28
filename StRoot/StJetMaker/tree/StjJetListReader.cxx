// $Id: StjJetListReader.cxx,v 1.1 2008/09/15 05:50:04 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetListReader.h"

#include <TTree.h>

#include <map>
#include <iostream>

ClassImp(StjJetListReader)

using namespace std;

void StjJetListReader::SetBranchAddress(TTree *jettree, TTree *fourtree)
{
  jettree->SetBranchAddress("eventId"    , &_jet_eventId      );
  jettree->SetBranchAddress("nJets"      , &_jet_nJets        );
  jettree->SetBranchAddress("jetId"      ,  _jet_jetId        );
  jettree->SetBranchAddress("pt"         ,  _jet_pt           );
  jettree->SetBranchAddress("eta"        ,  _jet_eta          );
  jettree->SetBranchAddress("detectorEta",  _jet_detectorEta  );
  jettree->SetBranchAddress("phi"        ,  _jet_phi          );
  jettree->SetBranchAddress("m"          ,  _jet_m            );
  jettree->SetBranchAddress("neuRt"      ,  _jet_neuRt        );
  jettree->SetBranchAddress("vertexZ"    , &_jet_vertexZ      );
  jettree->SetBranchAddress("runNumber"  , &_jet_runNumber    );

  fourtree->SetBranchAddress("eventId"     , &_four_eventId      );
  fourtree->SetBranchAddress("nFourVecs"   , &_four_nFourVecs    );
  fourtree->SetBranchAddress("jetId"       ,  _four_jetId        );     
  fourtree->SetBranchAddress("pt"          ,  _four_pt           );
  fourtree->SetBranchAddress("eta"         ,  _four_eta          );
  fourtree->SetBranchAddress("phi"         ,  _four_phi          );
  fourtree->SetBranchAddress("towerId"     ,  _four_towerId      );
  fourtree->SetBranchAddress("trackId"     ,  _four_trackId      );
  fourtree->SetBranchAddress("mcparticleId",  _four_mcparticleId );
  fourtree->SetBranchAddress("m"           ,  _four_m            );
  fourtree->SetBranchAddress("type"        ,  _four_type         );
  fourtree->SetBranchAddress("detectorId"  ,  _four_detectorId   );
  fourtree->SetBranchAddress("fourvecId"   ,  _four_fourvecId    );
  fourtree->SetBranchAddress("runNumber"   , &_four_runNumber    );
  fourtree->SetBranchAddress("vertexZ"     , &_four_vertexZ      );
}

void StjJetListReader::clearEntry()
{
  _list.clear();
}

void StjJetListReader::readEntry()
{
  clearEntry();
  for(int i = 0; i < _jet_nJets; ++i) {
    StjJet jet;
    jet.runNumber = _jet_runNumber;
    jet.eventId = _jet_eventId;
    jet.jetId = _jet_jetId[i];
    jet.pt = _jet_pt[i];
    jet.eta = _jet_eta[i];
    jet.phi = _jet_phi[i];
    jet.m = _jet_m[i];
    jet.neuRt = _jet_neuRt[i];
    jet.vertexZ = _jet_vertexZ;
    jet.detectorEta = _jet_detectorEta[i];

    for(int j = 0; j < _four_nFourVecs; ++j) {
      if(_four_jetId[j] != jet.jetId) continue;
      StjFourVec four;
      four.runNumber    = _four_runNumber;
      four.eventId      = _four_eventId;
      four.vertexZ      = _four_vertexZ;
      four.fourvecId    = _four_fourvecId[j];
      four.type         = _four_type[j];
      four.detectorId   = _four_detectorId[j];
      four.trackId      = _four_trackId[j];
      four.towerId      = _four_towerId[j];
      four.mcparticleId = _four_mcparticleId[j];
      four.pt           = _four_pt[j];
      four.eta          = _four_eta[j];
      four.phi          = _four_phi[j];
      four.m            = _four_m[j];
      jet.fourVecList.push_back(four);
    }
    _list.push_back(jet);
  }
}
