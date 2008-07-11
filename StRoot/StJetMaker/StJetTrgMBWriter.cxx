// $Id: StJetTrgMBWriter.cxx,v 1.1 2008/07/11 23:32:21 tai Exp $
#include "StJetTrgMBWriter.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

#include <TFile.h>
#include <TTree.h>

#include <map>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;


void StJetTrgMBWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  _tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  _tree->Branch("pass"       , &_pass         , "pass/I"         );

  _trigID = _trgId;

}

void StJetTrgMBWriter::Make()
{

  _pass = (_uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId));

  _runNumber = _uDstMaker->muDst()->event()->runId();
  _eventId = _uDstMaker->muDst()->event()->eventId();

  _prescale = StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];

  _tree->Fill();
}

void StJetTrgMBWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
