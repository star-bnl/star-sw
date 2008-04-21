// $Id: StConePars.cxx,v 1.2 2008/04/21 17:29:24 tai Exp $
#include "StConePars.h"

#include "StConeJetFinder.h"

ClassImp(StConePars)

StJetFinder* StConePars::constructJetFinder()
{
  return new StConeJetFinder(*this);
}
