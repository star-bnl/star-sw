// $Id: StjJetCutTrgBHT.cxx,v 1.1 2008/09/12 00:32:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetCutTrgBHT.h"

#include "StjFourVecCutTrgBHT.h"

#include <StjTrg.h>

ClassImp(StjJetCutTrgBHT)

StjJetCutTrgBHT::StjJetCutTrgBHT(StjTrg* trg) 
: _trg(trg)
{
  _fourVecListCut.addCut(new StjFourVecCutTrgBHT(_trg));
}

bool StjJetCutTrgBHT::operator()(const StjJet& jet)
{
  if( ! _trg->passed() ) return true;

  StjFourVecList fourPassed = _fourVecListCut(jet.fourVecList);

  if( fourPassed.empty() ) return true;

  return false;
}

