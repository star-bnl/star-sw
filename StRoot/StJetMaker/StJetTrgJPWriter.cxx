// $Id: StJetTrgJPWriter.cxx,v 1.5 2008/07/23 02:34:04 tai Exp $
#include "StJetTrgJPWriter.h"

#include "StJetTrg.h"

#include <TDirectory.h>
#include <TTree.h>

#include <vector>

using namespace std;


void StJetTrgJPWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  _tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  _tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  _tree->Branch("passed"     , &_passed       , "passed/I"         );
  _tree->Branch("hard"       , &_hard         , "hard/I"         );
  _tree->Branch("soft"       , &_soft         , "soft/I"         );
  _tree->Branch("nJetPatches", &_nJetPatches  , "nJetPatches/I"  );
  _tree->Branch("jetPatchId"   ,  _jetPatchId     , "jetPatchId[nJetPatches]/I");

  _trigID = _trgId;
}

void StJetTrgJPWriter::Make()
{
  _hard = _trg->hard(_trgId);
  _soft = _trg->soft(_trgId);

  if(!(_hard || _soft)) return;

  _passed = (_hard && _soft);

  _runNumber = _trg->runNumber();

  _eventId = _trg->eventId();

  _vertexZ = _trg->vertexZ();

  _prescale = _trg->prescale(_trgId);

  vector<int> jps = _trg->jetPatches(_trgId);

  _nJetPatches = jps.size();

  for(int i = 0; i < _nJetPatches; ++i) {
    _jetPatchId[i] = jps[i];
  }

  _tree->Fill();
}

void StJetTrgJPWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
