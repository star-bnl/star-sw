// $Id: StJetTrgWriter.cxx,v 1.2 2008/07/24 02:14:51 tai Exp $
#include "StJetTrgWriter.h"

#include "StJetTrg.h"

#include <TDirectory.h>
#include <TTree.h>

void StJetTrgWriter::Init()
{
  _tree = createTree();
  createBranch_general(_tree);
  createBranch_trgSpecific(_tree);
}

void StJetTrgWriter::Make()
{
  if( !(*_fillCondition)() ) return;
  fillBranch_general();
  fillBranch_trgSpecific();
  _tree->Fill();
}

void StJetTrgWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

TTree* StJetTrgWriter::createTree()
{
  _file->cd();
  return new TTree(_treeName.c_str(), _treeTitle.c_str());
}

void StJetTrgWriter::createBranch_general(TTree* tree)
{
  tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  tree->Branch("passed"     , &_passed       , "passed/I"       );
  tree->Branch("hard"       , &_hard         , "hard/I"         );
  tree->Branch("soft"       , &_soft         , "soft/I"         );
}

void StJetTrgWriter::fillBranch_general()
{
  _trigID = _trg->id();

  _hard = _trg->hard();

  _soft = _trg->soft();

  _passed = (*_passCondition)();

  _runNumber = _trg->runNumber();

  _eventId = _trg->eventId();

  _vertexZ = _trg->vertexZ();

  _prescale = _trg->prescale();
}

