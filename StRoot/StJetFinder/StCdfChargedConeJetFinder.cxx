// $Id: StCdfChargedConeJetFinder.cxx,v 1.20 2008/05/05 01:46:05 tai Exp $
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
  addToPrejets(*mWorkCell);
}

void StCdfChargedConeJetFinder::findJets_sub2()
{

}

bool StCdfChargedConeJetFinder::acceptSeed(const StEtaPhiCell* cell)
{
  return (cell->nTimesUsed()==0 && cell->empty()==false);
}

void StCdfChargedConeJetFinder::print()
{
}

bool StCdfChargedConeJetFinder::shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const
{
  if (otherCell.nTimesUsed()) return true;
  if (otherCell.empty()) return true;
  if (otherCell.eT() <= mPars.assocEtMin()) return true; 
  return false;
}

