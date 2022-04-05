// $Id: StjMCKinWriter.cxx,v 1.2 2008/10/14 17:16:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCKinWriter.h"

#include <StjMCKin.h>

#include <TDirectory.h>
#include <TTree.h>

ClassImp(StjMCKinWriter)

void StjMCKinWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("s"          , &_s            , "s/D"            );
  _tree->Branch("t"          , &_t            , "t/D"            );
  _tree->Branch("u"          , &_u            , "u/D"            );
  _tree->Branch("pid"        , &_pid          , "pid/I"          );
  _tree->Branch("pt"         , &_pt           , "pt/D"           );
  _tree->Branch("costh"      , &_costh        , "costh/D"        );
  _tree->Branch("x1"         , &_x1           , "x1/D"           );
  _tree->Branch("x2"         , &_x2           , "x2/D"           );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
}

void StjMCKinWriter::Make()
{
  _runNumber = _mckin->runNumber();

  _eventId = _mckin->eventId();

  _vertexZ = _mckin->vertexZ();

  _s       = _mckin->s();

  _t       = _mckin->t();

  _u       = _mckin->u();

  _pt      = _mckin->pt();

  _costh   = _mckin->costh();

  _x1      = _mckin->x1();

  _x2      = _mckin->x2();

  _pid     = _mckin->pid();

  _tree->Fill();
}

void StjMCKinWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

