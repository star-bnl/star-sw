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
  mPars= pars;
  buildGrid();
  mTheEnd = _EtCellList.end();
}

StCdfChargedConeJetFinder::~StCdfChargedConeJetFinder()
{
}

StJetEtCell* StCdfChargedConeJetFinder::makeCell(double etaMin, double etaMax,
						 double phiMin, double phiMax)
{
    return new StCdfChargedJetEtCell(etaMin, etaMax, phiMin, phiMax);
}

void StCdfChargedConeJetFinder::Init()
{
  //    mMerger->setSplitFraction(mPars.mSplitFraction);
  //    buildGrid();
  //    mTheEnd = _EtCellList.end();
}

void StCdfChargedConeJetFinder::findJets(JetList& protoJetList)
{
  clear();
  fillGrid(protoJetList);
  setSearchWindow();
  protoJetList.clear(); //clear 'em, add them back in as we find them
  
  //we partition them so that we don't waste operations on empty cells:
  mTheEnd = std::partition(_EtCellList.begin(), _EtCellList.end(), StJetEtCellIsNotEmpty());
  std::for_each(_EtCellList.begin(), _EtCellList.end(), PreJetUpdater() );
    
  //now we sort them in descending order in LcpPt
  //    std::sort(_EtCellList.begin(), mTheEnd, StJetEtCellEtGreaterThan() ); //This is ok, sorts by lcp-pt here
  _EtCellList.sort(StJetEtCellEtGreaterThan());
    
  if (mPars.mDebug ) {print();}
    
  //loop from highest lcp-pt cell to lowest lcp-pt cell.
  for (CellList::iterator vecIt = _EtCellList.begin(); vecIt != mTheEnd; ++vecIt) {
	
    StJetEtCell* centerCell = *vecIt;
    if (centerCell->eT() <= mPars.mSeedEtMin) break; //we're all done
	
    if (acceptSeed(centerCell)) {
      //new Seed

      //use a work object: mWorkCell
      initializeWorkCell(centerCell);
	    
      doSearch();
      addToPrejets(&mWorkCell);
    }
  }
    
  cout <<"\tcollect"<<endl;
  for (ValueCellList::iterator realJetIt=mPreJets.begin(); realJetIt!=mPreJets.end(); ++realJetIt) {
    StJetEtCell* rj = &(*realJetIt);
    if ( rj->cellList().size()>0 ) { //at least one non-empty cell in cone
      protoJetList.push_back( collectCell(rj) );
    }
  }
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
	     && otherCell->eT()>mPars.mAssocEtMin
	     
	     //within cone?
	     && centerCell->distance(*otherCell)<mPars.mR 
		
	     );
}
