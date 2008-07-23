// $Id: StJetTrgMBWriter.cxx,v 1.4 2008/07/23 02:34:05 tai Exp $
#include "StJetTrgMBWriter.h"

#include "StJetTrg.h"

#include <TDirectory.h>
#include <TTree.h>

using namespace std;

void StJetTrgMBWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  _tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  _tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  _tree->Branch("passed"     , &_passed       , "passed/I"         );

  _trigID = _trgId;

}

void StJetTrgMBWriter::Make()
{
  _passed = _trg->hard(_trgId);

  _runNumber = _trg->runNumber();

  _eventId = _trg->eventId();

  _vertexZ = _trg->vertexZ();

  _prescale = _trg->prescale(_trgId);

  _tree->Fill();
}

void StJetTrgMBWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
