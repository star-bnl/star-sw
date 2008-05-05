// $Id: StConeJetFinder.cxx,v 1.35 2008/05/05 14:49:53 tai Exp $
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
  mPreJets.clear();
  mSearchCounter = 0;

  _cellGrid.fillGridWith(protoJetList);

  if (mPars.debug()) {print();}

  CellList etSortedCellList = _cellGrid.EtSortedCellList();

  for (CellList::iterator etCell = etSortedCellList.begin(); etCell != etSortedCellList.end(); ++etCell) {

    if ((*etCell)->eT() <= mPars.seedEtMin()) break; //we're all done

    if (!acceptSeed((*etCell))) continue;

    //use a work object: mWorkCell
    initializeWorkCell((*etCell));

    findJets_sub1();
  }
    
  findJets_sub2();

  protoJetList.clear(); //clear 'em, add them back in as we find them
  
  for (ValueCellList::iterator realJetIt=mPreJets.begin(); realJetIt!=mPreJets.end(); ++realJetIt) {
    StEtaPhiCell* rj = &(*realJetIt);
    if ( rj->cellList().size()>0 ) { //at least one non-empty cell in cone
      protoJetList.push_back( collectCell(rj) );
    }
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
    mMerger->splitMerge(mPreJets);
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
    mWorkCell->add(*cell);
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
  return(_cellGrid.Cell(eta1, phi1) == _cellGrid.Cell(eta2, phi2));
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

  mPreJets.push_back(cell);
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
	
  mMidpointVec.clear();
	
  for (ValueCellList::iterator pj1 = mPreJets.begin(); pj1 != mPreJets.end(); ++pj1) {
    for (ValueCellList::iterator pj2 = pj1; pj2 != mPreJets.end(); ++pj2) {
      if (*pj1 == *pj2) continue;

      if ((*pj1).distance(*pj2) > 2.0*mPars.coneRadius()) continue;

      mMidpointVec.push_back(VCLItPairVec::value_type(pj1, pj2));

    }
  }
	
  for (VCLItPairVec::iterator it = mMidpointVec.begin(); it != mMidpointVec.end(); ++it) {

    mWorkCell->clear();
		
    StEtaPhiCell* mp = defineMidpoint(*(*it).first, *(*it).second );

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

void StConeJetFinder::print()
{
    cout <<"\nStConeJetFinder::print()"<<endl;
    cout <<"mNeta:\t"<<mPars.Neta()<<endl;
    cout <<"mNphi:\t"<<mPars.Nphi()<<endl;
    cout <<"mEtaMin:\t"<<mPars.EtaMin()<<endl;
    cout <<"mEtaMax:\t"<<mPars.EtaMax()<<endl;    
    cout <<"mPhiMin:\t"<<mPars.PhiMin()<<endl;
    cout <<"mPhiMax:\t"<<mPars.PhiMax()<<endl;
    cout <<"mR:\t"<<mPars.coneRadius()<<endl;
    cout <<"mAssocEtMin:\t"<<mPars.assocEtMin()<<endl;
    cout <<"mSeedEtMin:\t"<<mPars.seedEtMin()<<endl;
    cout <<"mphiWidth:\t"<<mPars.phiWidth()<<endl;
    cout <<"metaWidth:\t"<<mPars.etaWidth()<<endl;
    cout <<"mdeltaPhi:\t"<<mPars.deltaPhi()<<endl;
    cout <<"mdeltaEta:\t"<<mPars.deltaEta()<<endl;
    cout <<"mDoMinimization:\t"<<mPars.performMinimization()<<endl;
    cout <<"mAddMidpoints:\t"<<mPars.addMidpoints()<<endl;
    cout <<"mDoSplitMerge:\t"<<mPars.doSplitMerge()<<endl;
    cout <<"mSplitFraction:\t"<<mPars.splitFraction()<<endl;
    cout <<"splitFraction():\t"<<mMerger->splitFraction()<<endl;
    cout <<"mRequireStableMidpoints:\t"<<mPars.requiredStableMidpoints()<<endl;
    cout <<"mDebug:\t"<<mPars.debug()<<endl;
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

