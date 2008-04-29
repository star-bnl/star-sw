//M.L. Miller (Yale Software)
//Yale Software
//12/02

//std
#include <iostream>
#include <algorithm>
#include <time.h>
#include <map>
using std::multimap;
using std::for_each;
using std::sort;

//StJetFinder
#include "StJetEtCell.h"
#include "StJetSpliterMerger.h"
#include "StProtoJet.h"
#include "StCdfChargedJetEtCell.h"
#include "StCdfChargedConeJetFinder.h"

//careful, can't use buildGrid() call in base class constructor or we'll sef-fault because it calls
//a virtual function in the constructor while the derived class doesn't yet exist
StCdfChargedConeJetFinder::StCdfChargedConeJetFinder(const StCdfChargedConePars& pars) 
  : StConeJetFinder(pars)
{
  //  mPars= pars;
  //  buildGrid();
}

StCdfChargedConeJetFinder::~StCdfChargedConeJetFinder()
{
}

StJetEtCell* StCdfChargedConeJetFinder::makeCell(double etaMin, double etaMax,
						 double phiMin, double phiMax)
{
    return new StCdfChargedJetEtCell(etaMin, etaMax, phiMin, phiMax);
}

StJetEtCellFactory* StCdfChargedConeJetFinder::makeCellFactory()
{
  return new StCdfChargedJetEtCellFactory;
}

void StCdfChargedConeJetFinder::Init()
{
  //    mMerger->setSplitFraction(mPars.mSplitFraction);
  buildGrid();
}

void StCdfChargedConeJetFinder::findJets_sub1()
{
  doSearch();
  addToPrejets(&mWorkCell);
}

void StCdfChargedConeJetFinder::findJets_sub2()
{

}

bool StCdfChargedConeJetFinder::acceptSeed(const StJetEtCell* cell)
{
    return (cell->nTimesUsed()==0 && cell->empty()==false);
    //return (cell->empty()==false);
}

void StCdfChargedConeJetFinder::print()
{
}

bool StCdfChargedConeJetFinder::acceptPair(const StJetEtCell* centerCell,
					   const StJetEtCell* otherCell) const
{
    return  (
	     
	     //both valid
	     centerCell && otherCell 
	     
	     //don't add to self
	     //&& ( centerCell->eta()!=otherCell->eta() || centerCell->phi()!=otherCell->phi() )
	     //&& otherCell!=centerCell
	     
	     //allow non-unique assignment?
	     && otherCell->nTimesUsed()==0
	     
	     //no noise
	     && otherCell->empty()==false 
	     
	     //cut on associated eT
	     && otherCell->eT()>mPars.assocEtMin()
	     
	     //within cone?
	     && centerCell->distance(*otherCell)<mPars.coneRadius() 
		
	     );
}
