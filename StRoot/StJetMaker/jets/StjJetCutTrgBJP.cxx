// $Id: StjJetCutTrgBJP.cxx,v 1.1 2008/09/12 00:32:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetCutTrgBJP.h"

#include "StjFourVecCutTrgBJP.h"

#include <StjTrg.h>

ClassImp(StjJetCutTrgBJP)

StjJetCutTrgBJP::StjJetCutTrgBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap) 
: _trg(trg)
{
  _fourVecListCut.addCut(new StjFourVecCutTrgBJP(_trg, jetPatchTowerMap));
}

bool StjJetCutTrgBJP::operator()(const StjJet& jet)
{
  if( ! _trg->passed() ) return true;

  StjFourVecList fourPassed = _fourVecListCut(jet.fourVecList);

  if( fourPassed.empty() ) return true;

  return false;
}

