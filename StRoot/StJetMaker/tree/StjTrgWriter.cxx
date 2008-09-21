// $Id: StjTrgWriter.cxx,v 1.3 2008/09/21 19:11:40 tai Exp $
#include "StjTrgWriter.h"

#include "StjTrg.h"

#include <TDirectory.h>
#include <TTree.h>

ClassImp(StjTrgWriter)

void StjTrgWriter::Init()
{
  _tree = createTree();
  createBranch_general(_tree);
  createBranch_trgSpecific(_tree);
}

void StjTrgWriter::Make()
{
  if( !(*_fillCondition)(_trg) ) return;
  fillBranch_general();
  fillBranch_trgSpecific();
  _tree->Fill();
}

void StjTrgWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

TTree* StjTrgWriter::createTree()
{
  _file->cd();
  return new TTree(_treeName.c_str(), _treeTitle.c_str());
}

void StjTrgWriter::createBranch_general(TTree* tree)
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

void StjTrgWriter::fillBranch_general()
{
  _trigID = _trg->id();

  _hard = _trg->hard();

  _soft = _trg->soft();

  _passed = _trg->passed();

  _runNumber = _trg->runNumber();

  _eventId = _trg->eventId();

  _vertexZ = _trg->vertexZ();

  _prescale = _trg->prescale();
}

