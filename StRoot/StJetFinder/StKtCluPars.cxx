// $Id: StKtCluPars.cxx,v 1.2 2008/04/21 17:29:25 tai Exp $
#include "StKtCluPars.h"

#include "StKtCluJetFinder.h"

ClassImp(StKtCluPars)

StJetFinder* StKtCluPars::constructJetFinder()
{
  return new StKtCluJetFinder(*this);
}

