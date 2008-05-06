// $Id: StConeJetFinder.cxx,v 1.43 2008/05/06 22:43:48 tai Exp $
#include "StConeJetFinder.h"

#include "StJetSpliterMerger.h"

using namespace StSpinJet;

StConeJetFinder::StConeJetFinder(const StConePars& pars)
  : StConeJetFinderBase(pars)
  , mMerger(new StJetSpliterMerger())
{
    mMerger->setSplitFraction(mPars.splitFraction());
}

StConeJetFinder::~StConeJetFinder()
{

}

void StConeJetFinder::findJets(JetList& protoJetList)
{
  clearPreviousResult();

  CellList orderedList = generateEtOrderedList(protoJetList);

  CellList toSearchList = generateToSearchListFrom(orderedList);

  findProtoJets(toSearchList);

  if (mPars.addMidpoints()) {
    addSeedsAtMidpoint();
  }

  if (mPars.doSplitMerge()) {
    mMerger->splitMerge(_preJets);
  }

  storeTheResultIn(protoJetList);

}

void StConeJetFinder::findJetAroundThis(StEtaPhiCell* cell)
{
  initializeWorkCell(cell);
  
  if (mPars.performMinimization()) {
    doMinimization();
  } else {
    doSearch();
    addToPrejets(*mWorkCell);
  }

}

bool StConeJetFinder::shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const
{
  if (cell->empty()) return true;

  return false;
}

void StConeJetFinder::addSeedsAtMidpoint()
{
    //we have to first locate and remember pairs separated by d<2.*r.  
    //we then have to add midpoints.

  _cellPairList.clear();

  for (CellList::iterator pj1 = _preJets.begin(); pj1 != _preJets.end(); ++pj1) {
    for (CellList::iterator pj2 = pj1; pj2 != _preJets.end(); ++pj2) {

      if ((*pj1)->isSamePosition(**pj2)) continue;

      if ((*pj1)->distance(**pj2) > 2.0*mPars.coneRadius()) continue;

      _cellPairList.push_back(CellPairList::value_type(pj1, pj2));

    }
  }
	
  for (CellPairList::iterator it = _cellPairList.begin(); it != _cellPairList.end(); ++it) {

    mWorkCell->clear();
		
    StEtaPhiCell* mp = defineMidpoint(**(*it).first, **(*it).second );

    initializeWorkCell(mp);
	    
    mSearchCounter = 0;
    SearchResult res = doSearch();
    if (mPars.requiredStableMidpoints()) {
      if (res == kConverged) {
	addToPrejets(*mWorkCell);
      }	
    } else {
      addToPrejets(*mWorkCell);
    }
  }

}

double midpoint(double v1, double v2)
{
    double high, low;
    if (v1 > v2) {
      high =v1;
      low=v2;
    }
    else { 
      high = v2;
      low=v1;
    }
    return (high - low)/2. + low;
}

StEtaPhiCell* StConeJetFinder::defineMidpoint(const StEtaPhiCell& pj1, const StEtaPhiCell& pj2) 
{
    double etaMid = midpoint(pj1.eta(), pj2.eta());
    double phiMid = midpoint(pj1.phi(), pj2.phi());
    return _cellGrid.Cell(etaMid, phiMid);
}

void StConeJetFinder::doMinimization()
{
    mSearchCounter = 0;
	
    SearchResult res = kContinueSearch;
    while(res == kContinueSearch) {
		
      res = doSearch();
	
      if (res == kConverged) {
	addToPrejets(*mWorkCell);
	break;
      }			

      //find cell corresponding to centroid of cone
      StEtaPhiCell* newCenterCell = _cellGrid.Cell(mWorkCell->centroid().eta(), mWorkCell->centroid().phi());
      if (!newCenterCell) {
	cout << "newCenterCell doesn't exist.  key:\t" << endl;
	res = kLeftVolume;
      } else {
	initializeWorkCell(newCenterCell);
      }
    }

}

