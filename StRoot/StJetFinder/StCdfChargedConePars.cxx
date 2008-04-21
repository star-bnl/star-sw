// $Id: StCdfChargedConePars.cxx,v 1.2 2008/04/21 17:29:24 tai Exp $
#include "StCdfChargedConePars.h"

#include "StCdfChargedConeJetFinder.h"

ClassImp(StCdfChargedConePars)

StJetFinder* StCdfChargedConePars::constructJetFinder()
{
  return new StCdfChargedConeJetFinder(*this);
}

    
