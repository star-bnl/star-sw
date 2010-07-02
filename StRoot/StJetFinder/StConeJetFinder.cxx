// $Id: StConeJetFinder.cxx,v 1.53 2010/07/02 21:47:56 pibero Exp $
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
  delete mMerger;
}

StJetEtCellFactory* StConeJetFinder::makeCellFactory()
{
  return new StJetEtCellFactory;
}

void StConeJetFinder::findJets(JetList& protoJetList, const FourVecList& particleList)
{
  deleteToDelete();

  CellList orderedCellList = generateEtOrderedCellList(particleList);

  CellList toSearchCellList = generateToSearchListFrom(orderedCellList);

  CellList protoJetCellList = findProtoJetsAroundCells(toSearchCellList);

  if (mPars.addMidpoints()) {
    CellList midpointList = generateMidpointList(protoJetCellList);

    CellList midpointProtoJetCellList = findProtoJetsAroundMidpoints(midpointList);

    protoJetCellList.insert(protoJetCellList.end(), midpointProtoJetCellList.begin(), midpointProtoJetCellList.end());
  }

  if (mPars.doSplitMerge()) {
    mMerger->splitMerge(protoJetCellList);
  }

  storeTheResults(protoJetList, protoJetCellList);

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
  StEtaPhiCell* ret = cell.clone();
  _toDelete.push_back(ret);
  return ret;
}

void StConeJetFinder::deleteToDelete()
{
  for(vector<StEtaPhiCell*>::iterator it = _toDelete.begin(); it != _toDelete.end(); ++it)
    delete *it;

  _toDelete.clear();
}


bool StConeJetFinder::shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const
{
  if (cell->empty()) return true;

  return false;
}

StEtaPhiCell::CellList StConeJetFinder::generateMidpointList(const CellList& protoJetCellList)
{
  CellList midpointList;

  for (CellList::const_iterator pj1 = protoJetCellList.begin(); pj1 != protoJetCellList.end(); ++pj1) {
    for (CellList::const_iterator pj2 = pj1; pj2 != protoJetCellList.end(); ++pj2) {

      if ((*pj1)->isSamePosition(**pj2)) continue;

      if ((*pj1)->distance(**pj2) > 2.0*mPars.coneRadius()) continue;

      midpointList.push_back(_cellGrid.findMidpointCell(**pj1, **pj2));

    }
  }
  return midpointList;
}

StEtaPhiCell::CellList StConeJetFinder::findProtoJetsAroundMidpoints(CellList& midpointList)
{
  CellList ret;
  for (CellList::iterator it = midpointList.begin(); it != midpointList.end(); ++it) {

    initializeWorkCell(*it);
	    
    formCone();
    if (mPars.requiredStableMidpoints()) {
      const StProtoJet& centroid = mWorkCell->centroid();
      if (isInTheVolume(centroid.eta(), centroid.phi())) {
	if(areTheyInTheSameCell(mWorkCell->eta(), mWorkCell->phi(), centroid.eta(), centroid.phi())) {
	  ret.push_back(createJetCellFor(*mWorkCell));
	}	
      }
    } else {
	  ret.push_back(createJetCellFor(*mWorkCell));
    }
  }
  return ret;
}

bool StConeJetFinder::shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const
{
  if (otherCell.empty()) return true;
  if (otherCell.eT() <= mPars.assocEtMin()) return true; 
  return false;
}

void StConeJetFinder::storeTheResults(JetList& protoJetList, const CellList& protoJetCellList)
{
  protoJetList.clear();
  
  for (CellList::const_iterator jet = protoJetCellList.begin(); jet != protoJetCellList.end(); ++jet) {

    if ((*jet)->cellList().size() == 0) continue;

    protoJetList.push_back( collectCell(*jet) );
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



