// $Id: StjSpinReader.cxx,v 1.1 2008/11/05 05:48:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjSpinReader.h"

#include <TTree.h>

ClassImp(StjSpinReader)

void StjSpinReader::SetBranchAddress(TTree *tree)
{
  tree->SetBranchAddress("runNumber"   , &_runNumber       );
  tree->SetBranchAddress("eventId"     , &_eventId         );
  tree->SetBranchAddress("bx7"         , &_bx7             );
  tree->SetBranchAddress("bx48"        , &_bx48            );
  tree->SetBranchAddress("spin4"       , &_spin4           );
  tree->SetBranchAddress("bbcTimebin"  , &_bbcTimebin      );
  tree->SetBranchAddress("vertexZ"     , &_vertexZ         );
}

void StjSpinReader::clearEntry()
{
  __runNumber = 0;
  __eventId   = 0;
  __bx7 = 0;
  __bx48 = 0;
  __spin4 = 0;
  __bbcTimebin = 0;
  __vertexZ = 0;
}

void StjSpinReader::readEntry()
{
  __runNumber = _runNumber;
  __eventId = _eventId;
  __bx7 = _bx7;
  __bx48 = _bx48;
  __spin4 = _spin4;
  __bbcTimebin = _bbcTimebin;
  __vertexZ = _vertexZ;
}
