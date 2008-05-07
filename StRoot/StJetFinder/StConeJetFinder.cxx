// $Id: StConeJetFinder.cxx,v 1.46 2008/05/07 21:44:43 tai Exp $
#include "StConeJetFinder.h"

#include "StJetSpliterMerger.h"

#include <iostream>

using namespace std;
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
    CellList midpointList = generateMidpointList();

    findProtoJetsAroundMidpoints(midpointList);
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
    findJetWithStableCone();
  } else {
    formCone();
    addToPrejets(*mWorkCell);
  }

}

bool StConeJetFinder::shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const
{
  if (cell->empty()) return true;

  return false;
}

StEtaPhiCell::CellList StConeJetFinder::generateMidpointList()
{
  CellList midpointList;

  for (CellList::iterator pj1 = _preJets.begin(); pj1 != _preJets.end(); ++pj1) {
    for (CellList::iterator pj2 = pj1; pj2 != _preJets.end(); ++pj2) {

      if ((*pj1)->isSamePosition(**pj2)) continue;

      if ((*pj1)->distance(**pj2) > 2.0*mPars.coneRadius()) continue;

      midpointList.push_back(_cellGrid.findMidpointCell(**pj1, **pj2));

    }
  }
  return midpointList;
}

void StConeJetFinder::findProtoJetsAroundMidpoints(CellList& midpointList)
{
  for (CellList::iterator it = midpointList.begin(); it != midpointList.end(); ++it) {

    initializeWorkCell(*it);
	    
    formCone();
    if (mPars.requiredStableMidpoints()) {
      const StProtoJet& centroid = mWorkCell->centroid();
      if (isInTheVolume(centroid.eta(), centroid.phi())) {
	if(areTheyInTheSameCell(mWorkCell->eta(), mWorkCell->phi(), centroid.eta(), centroid.phi())) {
	  addToPrejets(*mWorkCell);
	}	
      }
    } else {
      addToPrejets(*mWorkCell);
    }
  }
}

void StConeJetFinder::findJetWithStableCone()
{

  static const int maxAttempt = 100;

  int _searchCounter = 0;
	
  while(1) {

    if (++_searchCounter > maxAttempt) break;
    
    formCone();
	
    const StProtoJet& centroid = mWorkCell->centroid();

    if (!isInTheVolume(centroid.eta(), centroid.phi())) break;

    if(areTheyInTheSameCell(mWorkCell->eta(), mWorkCell->phi(), centroid.eta(), centroid.phi())) {
      addToPrejets(*mWorkCell);
      break;
    }			

    StEtaPhiCell* newCenterCell = _cellGrid.Cell(mWorkCell->centroid().eta(), mWorkCell->centroid().phi());
    if (!newCenterCell) break;

    initializeWorkCell(newCenterCell);

  }

}


bool StConeJetFinder::isInTheVolume(double eta, double phi)
{
    return (_cellGrid.Cell(eta, phi)) ? true : false;
}

bool StConeJetFinder::areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2)
{
  return(_cellGrid.Cell(eta1, phi1)->isSamePosition(*_cellGrid.Cell(eta2, phi2)));
}



