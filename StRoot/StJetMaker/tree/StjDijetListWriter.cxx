// $Id: StjDijetListWriter.cxx,v 1.3 2008/09/19 23:19:21 tai Exp $
#include "StjDijetListWriter.h"

#include <TFile.h>
#include <TTree.h>

#include <iostream>

ClassImp(StjDijetListWriter)

using namespace std;

StjDijetListWriter::StjDijetListWriter(const char* treeName, TDirectory* file)
  : _file(file)
{
  _file->cd();

  _tree = new TTree(treeName, treeName);
  _tree->SetAutoSave(kMaxLong64);
  _tree->SetMaxTreeSize(kMaxLong64);

  _tree->Branch("eventId"    , &_eventId      , "eventId/I"   );
  _tree->Branch("dijetId"    , &_dijetId      , "dijetId/I"   );     
  _tree->Branch("m"          , &_m            , "m/D"         );
  _tree->Branch("eta"        , &_eta          , "eta/D"       );
  _tree->Branch("costh"      , &_costh        , "costh/D"     );
  _tree->Branch("deta"       , &_deta         , "deta/D"      );
  _tree->Branch("dphi"       , &_dphi         , "dphi/D"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"   );    
  _tree->Branch("jet3Id"     , &_jet3Id       , "jet3Id/I"    );     
  _tree->Branch("jet4Id"     , &_jet4Id       , "jet4Id/I"    );     
  _tree->Branch("jetSameSideId", &_jetSameSideId, "jetSameSideId/I"    );     
  _tree->Branch("jetAwaySideId", &_jetAwaySideId, "jetAwaySideId/I"    );     
  _tree->Branch("neuRtSame"  , &_neuRtSame    , "neuRtSame/D"    );     
  _tree->Branch("neuRtAway"  , &_neuRtAway    , "neuRtAway/D"    );     
  _tree->Branch("neuRt3"     , &_neuRt3       , "neuRt3/D"    );     
  _tree->Branch("neuRt4"     , &_neuRt4       , "neuRt4/D"    );     
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I" );
}

void StjDijetListWriter::Fill(const StjDijetList& dijetList)
{
  if(dijetList.empty()) return;

  _runNumber = dijetList[0].runNumber;
  _eventId   = dijetList[0].eventId;
  _dijetId   = dijetList[0].dijetId;
  _m         = dijetList[0].m;
  _eta       = dijetList[0].eta;
  _costh     = dijetList[0].costh;
  _deta      = dijetList[0].deta;
  _dphi      = dijetList[0].dphi;   
  _vertexZ   = dijetList[0].vertexZ;
  _jet3Id    = dijetList[0].jet3.jetId;
  _jet4Id    = dijetList[0].jet4.jetId;
  _jetSameSideId = dijetList[0].jetSameSide.jetId;
  _jetAwaySideId = dijetList[0].jetAwaySide.jetId;
  _neuRtSame = dijetList[0].neuRtSameSide;
  _neuRtAway = dijetList[0].neuRtAwaySide;
  _neuRt3    = dijetList[0].neuRt3;
  _neuRt4    = dijetList[0].neuRt4;

  _tree->Fill();
}

void StjDijetListWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
