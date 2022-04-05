// $Id: StjVertexWriter.cxx,v 1.1 2008/08/13 19:37:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjVertexWriter.h"

#include <StjVertex.h>

#include <TDirectory.h>
#include <TTree.h>

ClassImp(StjVertexWriter)

void StjVertexWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  _tree->Branch("vertexY"    , &_vertexY      , "vertexY/D"      );
  _tree->Branch("vertexX"    , &_vertexX      , "vertexX/D"      );
}

void StjVertexWriter::Make()
{
  _runNumber = _vertex->runNumber();

  _eventId = _vertex->eventId();

  _vertexZ = _vertex->vertexZ();

  _vertexY = _vertex->vertexY();

  _vertexX = _vertex->vertexX();

  _tree->Fill();
}

void StjVertexWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

