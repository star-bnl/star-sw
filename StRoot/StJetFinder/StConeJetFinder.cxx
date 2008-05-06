// $Id: StConeJetFinder.cxx,v 1.40 2008/05/06 18:26:18 tai Exp $
#include "StConeJetFinder.h"

#include "TObject.h"

//std
#include <iostream>
#include <algorithm>
#include <time.h>
#include <map>
using std::sort;

#include "StMessMgr.h"

//StJetFinder
#include "StJetEtCell.h"
#include "StJetSpliterMerger.h"
#include "StProtoJet.h"

using namespace StSpinJet;

StConeJetFinder::StConeJetFinder(const StConePars& pars)
  : mPars(pars)
  , mWorkCell(new StJetEtCell)
  , mSearchCounter(0)
  , mMerger(new StJetSpliterMerger())
  , _cellGrid(mPars)
{
    mMerger->setSplitFraction(mPars.splitFraction());
}

StConeJetFinder::~StConeJetFinder()
{

}

void StConeJetFinder::Init()
{
  _cellGrid.buildGrid(makeCellFactory());
}

StJetEtCellFactory* StConeJetFinder::makeCellFactory()
{
  return new StJetEtCellFactory;
}

void StConeJetFinder::findJets(JetList& protoJetList)
{
  clearPreviousResult();

  CellList orderedList = generateEtOrderedList(protoJetList);

  findProtoJets(orderedList);

  // midpoint split/merge
  findJets_sub2();

  protoJetList.clear();
  
  for (CellList::iterator jet = _preJets.begin(); jet != _preJets.end(); ++jet) {

    if ((*jet)->cellList().size() == 0) continue;

    protoJetList.push_back( collectCell(*jet) );
  }

}

void StConeJetFinder::clearPreviousResult()
{
  for(CellList::iterator it = _preJets.begin(); it != _preJets.end(); ++it) {
    delete *it;
  }
  _preJets.clear();
  mSearchCounter = 0;
}

StEtaPhiCell::CellList StConeJetFinder::generateEtOrderedList(JetList& protoJetList)
{
  _cellGrid.fillGridWith(protoJetList);
  return _cellGrid.EtSortedCellList();
}

void StConeJetFinder::findProtoJets(CellList& orderedList)
{
  for (CellList::iterator cell = orderedList.begin(); cell != orderedList.end(); ++cell) {

    if ((*cell)->eT() <= mPars.seedEtMin()) break;

    if (!acceptSeed((*cell))) continue;

    initializeWorkCell((*cell));

    findJets_sub1();

  }
}

bool StConeJetFinder::acceptSeed(const StEtaPhiCell* cell)
{
    return (cell->empty()==false);
}

void StConeJetFinder::initializeWorkCell(const StEtaPhiCell* other)
{
    mWorkCell->clear();
    *mWorkCell = *other;
    mWorkCell->setEt(0.);
    if (mWorkCell->cellList().empty()==false) {
	cout <<"StConeJetFinder::initializeWorkCell(). ERROR:\t"
	     <<"workCell is not empty. abort()"<<endl;
	abort();
    }
}


void StConeJetFinder::findJets_sub1()
{
  if (mPars.performMinimization()) {
    doMinimization();
  } else {
    doSearch();
    addToPrejets(*mWorkCell);
  }
}

void StConeJetFinder::findJets_sub2()
{
  if (mPars.addMidpoints()) { 	//add seeds at midpoints
    addSeedsAtMidpoint(); //old style, add midpoints before split/merge
  }

    
  if (mPars.doSplitMerge()) {//split-merge
    mMerger->splitMerge(_preJets);
  }
	
}

StConeJetFinder::SearchResult StConeJetFinder::doSearch()
{
    
  ++mSearchCounter;

  if (mPars.performMinimization() && mSearchCounter > 100) {
    return kTooManyTries;
  }
    
  CellList cellList = _cellGrid.WithinTheConeRadiusCellList(*mWorkCell);

  for (CellList::iterator cell = cellList.begin(); cell != cellList.end(); ++cell) {
    if(shouldNotAddToTheCell(*mWorkCell, **cell)) continue;
    mWorkCell->addCell(*cell);
  }

  const StProtoJet& centroid = mWorkCell->centroid();

  if (!isInTheVolume(centroid.eta(), centroid.phi())) return kLeftVolume;

  if(areTheyInTheSameCell(mWorkCell->eta(), mWorkCell->phi(), centroid.eta(), centroid.phi())) return kConverged;

  return kContinueSearch;
}

bool StConeJetFinder::isInTheVolume(double eta, double phi)
{
    return (_cellGrid.Cell(eta, phi)) ? true : false;
}

bool StConeJetFinder::shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const
{
  if (otherCell.empty()) return true;
  if (otherCell.eT() <= mPars.assocEtMin()) return true; 
  return false;
}

bool StConeJetFinder::areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2)
{
  return(_cellGrid.Cell(eta1, phi1)->isSamePosition(*_cellGrid.Cell(eta2, phi2)));
}

void StConeJetFinder::addToPrejets(StEtaPhiCell& cell)
{
  StEtaPhiCell* realCell = _cellGrid.Cell(cell.eta(), cell.phi());
  if (!realCell) {
    cout << "PreJetInitializer(). ERROR:\t"
	 << "real Cell doesn't exist." << endl;
    abort();
  }
	
  cell.setEt(0);
  for(CellList::iterator etCell = cell.cellList().begin(); etCell != cell.cellList().end(); ++etCell) {
    (*etCell)->update();
    cell.setEt(cell.Et() + (*etCell)->eT());
  }

  //  _preJets.push_back(new StEtaPhiCell(cell));
  _preJets.push_back(cell.clone());

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

const StProtoJet& StConeJetFinder::collectCell(StEtaPhiCell* seed)
{

  if (seed->cellList().empty()) {
    StProtoJet& center = seed->protoJet();
    center.update();
    cout <<"\treturn w/o action.  empty cell"<<endl;
    return center;
  }
    
	
  //arbitrarily choose protojet from first cell in list
  CellList& cells = seed->cellList();
  StEtaPhiCell* centerCell = cells.front();
  StProtoJet& center = centerCell->protoJet();
	
		
  //now combine protojets from other cells
  for (CellList::iterator it = cells.begin(); it != cells.end(); ++it) {
    if (it != cells.begin()) { //don't double count first!
      StEtaPhiCell* cell = (*it);
      if (!cell) {
	cout <<"\tStConeJetFinder::collectCell(). ERROR:\t"
	     <<"null cell.  skip"<<endl;
      }
      if (centerCell==*it) {
	cout <<"\tStConeJetFinder::collectCell(). ERROR:\t"
	     <<"attempt to add self! skip"<<endl;
      }	else {
	//cout <<"\t\tadding cell:\t"<<icell++<<endl;
	if (cell->empty()==false) {
	  center.add( cell->protoJet() );
	}
      }
    }
  }
  center.update();
  return center;
}



//non members ---

void PreJetLazyUpdater ::operator()(StEtaPhiCell& cell)
{
    sumEt += cell.eT();
}

void PreJetLazyUpdater ::operator()(StEtaPhiCell* cell)
{
    (*this)(*cell);
}

void PostMergeUpdater::operator()(StEtaPhiCell& cell)
{
    //now update each cell:
    PreJetLazyUpdater updater;
    updater = for_each( cell.cellList().begin(), cell.cellList().end(), updater );
	
    //now update jet-eT
    cell.mEt = updater.sumEt;
}

