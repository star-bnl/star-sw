// $Id: StjSpinWriter.cxx,v 1.1 2008/11/05 05:48:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjSpinWriter.h"

#include <StjSpin.h>

#include <TDirectory.h>
#include <TTree.h>

ClassImp(StjSpinWriter)

void StjSpinWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("bx7"        , &_bx7          , "bx7/I"      );
  _tree->Branch("bx48"       , &_bx48         , "bx48/I"      );
  _tree->Branch("spin4"      , &_spin4        , "spin4/I"      );
  _tree->Branch("bbcTimebin" , &_bbcTimebin   , "bbcTimebin/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
}

void StjSpinWriter::Make()
{
  _runNumber = _spin->runNumber();
  _eventId = _spin->eventId();
  _bx7 = _spin->bx7();       
  _bx48 = _spin->bx48();
  _spin4 = _spin->spin4();
  _bbcTimebin = _spin->bbcTimebin();
  _vertexZ = _spin->vertexZ();
  _tree->Fill();
}

void StjSpinWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

