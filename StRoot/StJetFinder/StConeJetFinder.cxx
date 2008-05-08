// $Id: StConeJetFinder.cxx,v 1.49 2008/05/08 04:07:23 tai Exp $
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

void StConeJetFinder::findJets(JetList& protoJetList, const FourVecList& particleList)
{
  clearPreviousResult();

  CellList orderedCellList = generateEtOrderedCellList(particleList);

  CellList toSearchCellList = generateToSearchListFrom(orderedCellList);

  CellList protoJetCellList = findProtoJetsAroundCells(toSearchCellList);

  //  cout << "++++++++++++++++++++++++++++++++++++++++ " << (protoJetCellList.size() == _preJets.size()) <<endl;

  if (mPars.addMidpoints()) {
    CellList midpointList = generateMidpointList();

    findProtoJetsAroundMidpoints(midpointList);
  }

  if (mPars.doSplitMerge()) {
    mMerger->splitMerge(_preJets);
  }

  storeTheResultIn(protoJetList);

}

StEtaPhiCell::CellList StConeJetFinder::generateEtOrderedCellList(const FourVecList& particleList)
{
  JetList protoJetList;
  
  for(FourVecList::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
    protoJetList.push_back(StProtoJet(*particle));
  }

  _cellGrid.fillGridWith(protoJetList);
  return _cellGrid.EtSortedCellList();
}

StEtaPhiCell::CellList StConeJetFinder::findProtoJetsAroundCells(CellList& toSearchList)
{
  CellList protoJetCellList;

  for (CellList::iterator cell = toSearchList.begin(); cell != toSearchList.end(); ++cell) {
  
    if (shouldNotSearchForJetAroundThis((*cell))) continue;

    if(StEtaPhiCell* jetCell = findJetAroundThis(*cell)) {
      protoJetCellList.push_back(jetCell);
    }
  }

  return protoJetCellList;
}


StEtaPhiCell* StConeJetFinder::findJetAroundThis(StEtaPhiCell* cell)
{
  initializeWorkCell(cell);
  
  if (mPars.performMinimization()) {
    return findJetWithStableCone();
  } else {
    formCone();
    addToPrejets(*mWorkCell);
    return createJetCellFor(*mWorkCell);
  }

}

StEtaPhiCell* StConeJetFinder::createJetCellFor(StEtaPhiCell& cell)
{
  cell.setEt(0);
  for(CellList::iterator etCell = cell.cellList().begin(); etCell != cell.cellList().end(); ++etCell) {
    (*etCell)->update();
    cell.setEt(cell.Et() + (*etCell)->eT());
  }
  return cell.clone();
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

StEtaPhiCell* StConeJetFinder::findJetWithStableCone()
{
  StEtaPhiCell* ret(0);

  static const int maxAttempt = 100;

  int _searchCounter = 0;
	
  while(1) {

    if (++_searchCounter > maxAttempt) break;
    
    formCone();
	
    const StProtoJet& centroid = mWorkCell->centroid();

    if (!isInTheVolume(centroid.eta(), centroid.phi())) break;

    if(areTheyInTheSameCell(mWorkCell->eta(), mWorkCell->phi(), centroid.eta(), centroid.phi())) {
      addToPrejets(*mWorkCell);
      ret = createJetCellFor(*mWorkCell);
      break;
    }			

    StEtaPhiCell* newCenterCell = _cellGrid.Cell(mWorkCell->centroid().eta(), mWorkCell->centroid().phi());
    if (!newCenterCell) break;

    initializeWorkCell(newCenterCell);

  }

  return ret;
}


bool StConeJetFinder::isInTheVolume(double eta, double phi)
{
    return (_cellGrid.Cell(eta, phi)) ? true : false;
}

bool StConeJetFinder::areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2)
{
  return(_cellGrid.Cell(eta1, phi1)->isSamePosition(*_cellGrid.Cell(eta2, phi2)));
}



