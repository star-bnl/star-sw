// $Id: StCdfChargedConeJetFinder.cxx,v 1.18 2008/04/30 00:23:31 tai Exp $
#include "StCdfChargedConeJetFinder.h"

#include "StJetEtCell.h"
#include "StJetSpliterMerger.h"
#include "StProtoJet.h"
#include "StCdfChargedJetEtCell.h"

StCdfChargedConeJetFinder::StCdfChargedConeJetFinder(const StCdfChargedConePars& pars) 
  : StConeJetFinder(pars)
{

}

StCdfChargedConeJetFinder::~StCdfChargedConeJetFinder()
{
}

StJetEtCellFactory* StCdfChargedConeJetFinder::makeCellFactory()
{
  return new StCdfChargedJetEtCellFactory;
}

void StCdfChargedConeJetFinder::findJets_sub1()
{
  doSearch();
  addToPrejets(mWorkCell);
}

void StCdfChargedConeJetFinder::findJets_sub2()
{

}

bool StCdfChargedConeJetFinder::acceptSeed(const StJetEtCell* cell)
{
  return (cell->nTimesUsed()==0 && cell->empty()==false);
}

void StCdfChargedConeJetFinder::print()
{
}

bool StCdfChargedConeJetFinder::shouldNotAddToTheCell(const StJetEtCell& theCell, const StJetEtCell& otherCell) const
{
  if (otherCell.nTimesUsed()) return true;
  if (otherCell.empty()) return true;
  if (otherCell.eT() <= mPars.assocEtMin()) return true; 
  return false;
}

