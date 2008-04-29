//#if defined(WIN32)
// $Id: StConeJetFinder.cxx,v 1.27 2008/04/29 01:55:52 tai Exp $
#include "StConeJetFinder.h"

#include "TObject.h"

//std
#include <iostream>
#include <algorithm>
#include <time.h>
#include <map>
using std::multimap;
using std::for_each;
using std::sort;

#include "StMessMgr.h"

//StJetFinder
#include "StJetEtCell.h"
#include "StJetSpliterMerger.h"
#include "StProtoJet.h"

StConeJetFinder::StConeJetFinder(const StConePars& pars)
  : mPars(pars)
  , mSearchCounter(0)
  , mMerger(new StJetSpliterMerger())
  , _cellGrid(mPars)
  , _EtCellMap(_cellGrid.EtCellMap())
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
    StJetEtCell* rj = &(*realJetIt);
    if ( rj->cellList().size()>0 ) { //at least one non-empty cell in cone
      protoJetList.push_back( collectCell(rj) );
    }
  }
}

void StConeJetFinder::findJets_sub1()
{
  if (mPars.performMinimization()) {
    doMinimization();
  } else {
    doSearch();
    addToPrejets(&mWorkCell);
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
    
  //now search for clusters!  We walk in a square in eta-phi space, testing 
  //each cluster for it's distance from the centerCell
  //define the search window as phi +- deltaPhi, same for eta
  //The walk in eta is easy, strictly linear.
  //The walk in phi is just a little trickier, since phi is periodic

  //find this entry in map (this is a double check, not really necessary)
  StEtGridKey centerKey = findKey(mWorkCell.eta(), mWorkCell.phi());
	
  //begin walk in eta:

  int iEtaMin = centerKey.eta() - mPars.deltaEta();
  if (iEtaMin < 0) iEtaMin = 0 ; //avoid wasted searches in eta
	
  for (int iEta = iEtaMin; (iEta <= centerKey.eta() + mPars.deltaEta()) && (iEta < mPars.Neta()); ++iEta) {
		
    //begin walk in phi:
    for (int iPhi = centerKey.phi() - mPars.deltaPhi(); iPhi <= centerKey.phi() + mPars.deltaPhi(); ++iPhi) {
			
      int iModPhi = iPhi;
      if (iModPhi < 0) iModPhi = iModPhi + mPars.Nphi();
      if (iModPhi >= mPars.Nphi()) iModPhi = iModPhi - mPars.Nphi();
			
      StJetEtCell* otherCell = findCellByKey(StEtGridKey(iEta, iModPhi));
      if (!otherCell) {
	//	cout <<"otherCell doesn't exist.  key:\t"<<key<<endl;
      }
			
      if (acceptPair(&mWorkCell, otherCell) ) {//add it!
	//cout <<"Adding cell w/ key:\t"<<key<<endl;
	mWorkCell.add(otherCell);
      }
    }
  }
	
  //finished with walk, compare centroid to cone center
  const StProtoJet& centroid = mWorkCell.centroid();

  if (!inVolume(centroid.eta(), centroid.phi())) return kLeftVolume;

  StEtGridKey centroidKey = findKey(centroid.eta(), centroid.phi());
	
  if (centroidKey == centerKey) return kConverged;

  return kContinueSearch;
}

void StConeJetFinder::addToPrejets(StJetEtCell* cell)
{
    PreJetInitializer initializer(*this);
    initializer(*cell); //test, try to initialize as we add
    mPreJets.push_back(*cell);
}

void StConeJetFinder::initializeWorkCell(const StJetEtCell* other)
{
    mWorkCell.clear();
    mWorkCell = *other;
    mWorkCell.setEt(0.);
    if (mWorkCell.cellList().empty()==false) {
	cout <<"StConeJetFinder::initializeWorkCell(). ERROR:\t"
	     <<"workCell is not empty. abort()"<<endl;
	abort();
    }
}

void StConeJetFinder::doMinimization()
{
    mSearchCounter = 0;
	
    SearchResult res = kContinueSearch;
    while(res == kContinueSearch) {
		
      res = doSearch();
	
      if (res == kConverged) {
	addToPrejets(&mWorkCell);
	break;
      }			

      //find cell corresponding to centroid of cone
      StEtGridKey key = findKey(mWorkCell.centroid().eta(), mWorkCell.centroid().phi() );
      StJetEtCell* newCenterCell = findCellByKey(key);
      if (!newCenterCell) {
	cout <<"newCenterCell doesn't exist.  key:\t"<<key<<endl;
	res = kLeftVolume;
      } else {
	initializeWorkCell(newCenterCell);
      }
    }

}

double midpoint(double v1, double v2)
{
    double high, low;
    if (v1>v2) {high=v1; low=v2;}
    else {high=v2; low=v1;}
    return (high-low)/2. + low;
}

//void StConeJetFinder::defineMidpoint(const StJetEtCell& pj1, const StJetEtCell& pj2, StJetEtCell& workCell) 
StJetEtCell* StConeJetFinder::defineMidpoint(const StJetEtCell& pj1, const StJetEtCell& pj2) 
{
    double etaMid = midpoint(pj1.eta(), pj2.eta() );
    double phiMid = midpoint(pj1.phi(), pj2.phi() );
    return findCellByKey( findKey(etaMid, phiMid) ); //can be void!
}

//add a seed at the midpoint between any two jets separated by d<2.*coneRadius();
void StConeJetFinder::addSeedsAtMidpoint()
{
    //we have to first locate and remember pairs separated by d<2.*r.  
    //we then have to add midpoints.  You cannot add the midpoint when you locate the 
    //pair without invalidating the iterators that you are using to control your loop
    //That's why we need two loops.  Even an algorithm call to for_each will not advert this.
	
    //cout <<"--- StConeJetFinder::addSeedsAtMidpoint() ---"<<endl;
	
    mMidpointVec.clear();
	
    //cout <<"------------------ conents of PreJets: ---------------"<<endl;
    for (ValueCellList::iterator pj1=mPreJets.begin(); pj1!=mPreJets.end(); ++pj1) {
	//cout <<*pj1<<endl;
	for (ValueCellList::iterator pj2=pj1; pj2!=mPreJets.end(); ++pj2) {
	    //don't double count.  ignore same iterators and jets w/ same center location
	    if (pj1!=pj2 && (*pj1==*pj2)==false ) { 
		if ( (*pj1).distance(*pj2) <= 2.* mPars.coneRadius()) { 
		    //remember this combination
		    VCLItPairVec::value_type temp(pj1, pj2);
		    mMidpointVec.push_back(temp);
		}				
	    }
	}
    }
	
    //cout <<"StConeJetFinder::addSeedsAtMidpoint().  Adding:\t"<<mMidpointVec.size()<<"\tseeds"<<endl;
	
    for (VCLItPairVec::iterator it=mMidpointVec.begin(); it!=mMidpointVec.end(); ++it) {
	//clear the work cell
	mWorkCell.clear();
		
	//see if the midpoint is within the volume.  Should always be true, but double check
	StJetEtCell* mp = defineMidpoint(*(*it).first, *(*it).second );
	if (0) {
	    cout <<"--- new midpoint:"<<endl;
	    cout <<"midpoint j1:\t"<<*(*it).first<<endl;
	    cout <<"midpoint j2:\t"<<*(*it).second<<endl;
	}
	if (mp) {

	    //initialize the work cell to the midpoint
	    initializeWorkCell(mp);
	    
	    //add in the cone
	    mSearchCounter=0;
	    SearchResult res = doSearch();
	    if (mPars.requiredStableMidpoints()) {
		if (res == kConverged) {
		    addToPrejets(&mWorkCell);
		}	
	    }	
	    else {
		addToPrejets(&mWorkCell);
	    }
	}
	else {
	    LOG_ERROR <<"StConeJetFinder::addSeedsAtMidpoint(). ERROR:\t"
		 <<"midpoint is null.  This should never happen"<<endm;
	    abort();
	}
    }
}

bool StConeJetFinder::inVolume(double eta, double phi)
{
    int iEta = findEtaKey(eta);
    int iPhi = findPhiKey(phi);
    if (iEta < 0 || iPhi < 0) {
	return false;
    }
    //const StJetEtCell* cell = findCellByKey( StEtGridKey(iEta, iPhi) );
    return (findCellByKey( StEtGridKey(iEta, iPhi) )) ? true : false;
}

StEtGridKey StConeJetFinder::findKey(double eta, double phi) const
{
  int iEta = findEtaKey(eta);
  int iPhi = findPhiKey(phi);
  if (iEta < 0 || iPhi < 0) {
    cout <<"StConeJetFinder::findKey(double, double). ERROR:\t"
	 <<"eta:\t"<<eta<<"\tphi:\t"<<phi<<"\t"
	 <<"iEta<0|| iPhi<0\tabort()"<<endl;
    //abort();
  }
  return StEtGridKey(iEta, iPhi);
}

int StConeJetFinder::findEtaKey(double eta) const
{
  return int((mPars.Neta()/(mPars.EtaMax() - mPars.EtaMin()))*(eta - mPars.EtaMin()));
}

int StConeJetFinder::findPhiKey(double phi) const
{
  while(phi > M_PI) phi -= 2*M_PI;
  while(phi < -M_PI) phi += 2*M_PI;
  return int( mPars.Nphi()*((phi - mPars.PhiMin())/(mPars.PhiMax() - mPars.PhiMin())));
}

bool StConeJetFinder::acceptSeed(const StJetEtCell* cell)
{
    return (cell->empty()==false);
}

bool StConeJetFinder::acceptPair(const StJetEtCell* centerCell, const StJetEtCell* otherCell) const
{
  if (!centerCell) return false;
  if (!otherCell) return false;
  if (otherCell->empty()) return false;
  if (otherCell->eT() <= mPars.assocEtMin()) return false; 
  if (centerCell->distance(*otherCell) >= mPars.coneRadius()) return false; 
  return true;
}


const StProtoJet& StConeJetFinder::collectCell(StJetEtCell* seed)
{
    //cout <<"StConeJetFinder::collectCell()"<<endl;
    if (seed->cellList().empty()==true) {
	StProtoJet& center = seed->protoJet();
	center.update();
	cout <<"\treturn w/o action.  empty cell"<<endl;
	return center;
    }
    
    else {
	
	//arbitrarily choose protojet from first cell in list
	CellList& cells = seed->cellList();
	StJetEtCell* centerCell = cells.front();
	StProtoJet& center = centerCell->protoJet();
	
	//cout <<"\tcombine protojets from other cells"<<endl;
	//int icell=0;
		
	//now combine protojets from other cells
	for (CellList::iterator it=cells.begin(); it!=cells.end(); ++it) {
	    if (it!=cells.begin()) { //don't double count first!
		StJetEtCell* cell = (*it);
		if (!cell) {
		    cout <<"\tStConeJetFinder::collectCell(). ERROR:\t"
			 <<"null cell.  skip"<<endl;
		}
		if (centerCell==*it) {
		    cout <<"\tStConeJetFinder::collectCell(). ERROR:\t"
			 <<"attempt to add self! skip"<<endl;
		}
		else {
		    //cout <<"\t\tadding cell:\t"<<icell++<<endl;
		    if (cell->empty()==false) {
			center.add( cell->protoJet() );
		    }
		}
	    }
	}
	//cout <<"\tupdate protojet"<<endl;
	center.update();
	//double pet = center.eT();
	//cout <<"\tcet:\t"<<cet<<"\tpet:\t"<<pet<<endl;
	//cout <<"\tfinished"<<endl;
	return center;
    }
}

StJetEtCell* StConeJetFinder::findCellByKey(const StEtGridKey& key)
{
    CellMap::iterator where = _EtCellMap.find(key);
    return (where!=_EtCellMap.end()) ? (*where).second : 0;
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

StConeJetFinder::CellMap::iterator 
StConeJetFinder::findIterator(double eta, double phi)
{
    StEtGridKey centerKey = findKey( eta, phi );
    return findIterator(centerKey);
}


StConeJetFinder::CellMap::iterator StConeJetFinder::findIterator(const StEtGridKey& centerKey)
{
    CellMap::iterator where = _EtCellMap.find(centerKey);
    if (where==_EtCellMap.end() ) {
	cout <<"StConeJetFinder::findIterator(). ERROR:\t"
	     <<"Mismatch in containers.  Calling abort"<<endl;
	abort();
    }
    return where;
}

//non members ---

void PreJetLazyUpdater ::operator()(StJetEtCell& cell)
{
    sumEt += cell.eT();
}

void PreJetLazyUpdater ::operator()(StJetEtCell* cell)
{
    (*this)(*cell);
}

void PostMergeUpdater::operator()(StJetEtCell& cell)
{
    //now update each cell:
    PreJetLazyUpdater updater;
    updater = for_each( cell.cellList().begin(), cell.cellList().end(), updater );
	
    //now update jet-eT
    cell.mEt = updater.sumEt;
}

void PreJetInitializer::operator()(StJetEtCell& cell) 
{
	
  //first find the pointer to the cell in the grid (not this local copy)
  StEtGridKey key = mConeFinder.findKey(cell.eta(), cell.phi() );
  StJetEtCell* realCell = mConeFinder.findCellByKey(key);
	
  if (!realCell) {
    cout <<"PreJetInitializer(). ERROR:\t"
	 <<"real Celldoesn't exist.  key:\t"<<key<<"\tabort()"<<endl;
    abort();
  }
	
  //now add this into the cone-cell collection, *if* it's not already there
  //this shouldn't happen, temp check to get rid of relic bug
  StConeJetFinder::CellList& cells = cell.cellList();
  StConeJetFinder::CellList::iterator where = std::find(cells.begin(), cells.end(), realCell);
  if (realCell->empty()==false) {
    if (where==cells.end()) {
      cout <<"\tADDING SELF IN CONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
      cell.add(realCell);
    }
  }
	
  cell.mEt = 0;
  for(StConeJetFinder::CellList::iterator etCell = cell.cellList().begin(); etCell != cell.cellList().end(); ++etCell) {
    (*etCell)->update();
    cell.mEt += (*etCell)->eT();
  }


}

