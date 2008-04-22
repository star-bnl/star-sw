//#if defined(WIN32)
// $Id: StConeJetFinder.cxx,v 1.12 2008/04/22 22:22:05 tai Exp $
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
{
    mMerger->setSplitFraction(mPars.mSplitFraction);
    buildGrid();
    mTheEnd = mVec.end();
}

StConeJetFinder::~StConeJetFinder()
{
    delete mMerger;
    mMerger=0;
    clearAndDestroy();
}

void StConeJetFinder::clearAndDestroy()
{
    for (CellList::iterator it=mVec.begin(); it!=mVec.end(); ++it) {
	delete *it;
	*it=0;
    }
}


void StConeJetFinder::Init()
{
//    mMerger->setSplitFraction(mPars.mSplitFraction);
//    buildGrid();
//    mTheEnd = mVec.end();
}

//StConeJetFinder::SearchResult StConeJetFinder::doSearch(StJetEtCell* centerCell)
StConeJetFinder::SearchResult StConeJetFinder::doSearch()
{
    //cout <<"--- new cell ---"<<endl;
    
    ++mSearchCounter;
    if (mPars.mDoMinimization==true && mSearchCounter>100) {
	return kTooManyTries;
    }
    
    //now search for clusters!  We walk in a square in eta-phi space, testing 
    //each cluster for it's distance from the centerCell
    //define the search window as phi +- deltaPhi, same for eta
    //The walk in eta is easy, strictly linear.
    //The walk in phi is just a little trickier, since phi is periodic
	
    //find this entry in map (this is a double check, not really necessary)
    StEtGridKey centerKey = findKey( mWorkCell.eta(), mWorkCell.phi() );
	
    //begin walk in eta:
    //cout <<"\nClustering around key:\t"<<centerKey<<endl;
	
    int iEtaMin = centerKey.iEta-mPars.mdeltaEta;
    if (iEtaMin<0) iEtaMin=0; //avoid wasted searches in eta
	
    for (int iEta=iEtaMin; (iEta<=centerKey.iEta+mPars.mdeltaEta) && (iEta<mPars.mNeta); ++iEta) {
		
	//begin walk in phi:
	for (int iPhi=centerKey.iPhi-mPars.mdeltaPhi; iPhi<=centerKey.iPhi+mPars.mdeltaPhi; ++iPhi) {
			
	    int iModPhi=iPhi;
	    if (iModPhi<0) iModPhi= iModPhi+mPars.mNphi;
	    if (iModPhi>=mPars.mNphi) iModPhi = iModPhi-mPars.mNphi;
			
	    StEtGridKey key(iEta, iModPhi);
			
	    StJetEtCell* otherCell = findCellByKey(key);
	    if (!otherCell) {
		cout <<"otherCell doesn't exist.  key:\t"<<key<<endl;
	    }
			
	    if (acceptPair(&mWorkCell, otherCell) ) {//add it!
		//cout <<"Adding cell w/ key:\t"<<key<<endl;
		mWorkCell.add(otherCell);
	    }
	}
    }
	
    //finished with walk, compare centroid to cone center
    const StFourVec& centroid = mWorkCell.centroid();
    StEtGridKey centroidKey;
	
    if (this->inVolume(centroid.eta(), centroid.phi() )) {
	centroidKey = findKey( centroid.eta(), centroid.phi() );
    }
    else {
	return kLeftVolume;
    }
	
    //temp debug
    /*
      cout <<"c_phi:   \t"<<centroid.phi()<<"\tc_eta:      "<<centroid.eta()<<endl;
      cout <<"cell_phi:\t"<<mWorkCell.phi()<<"\tcell_eta:\t"<<mWorkCell.eta()<<endl;
      cout <<"centroid key:\t"<<centroidKey<<endl;
      cout <<"cell key:\t"<<centerKey<<endl;
    */
	
    //enum SearchResult {kTooManyTries, kLeftVolume, kConverged, kContinueSearch};
    if (centroidKey==centerKey) {
	//cout <<"CONVERGED WITH:\t"<<mWorkCell.cellList().size()<<"\tcells in cone"<<endl;
	return kConverged;
    }
    else {
	return kContinueSearch;
    }
}

void StConeJetFinder::addToPrejets(StJetEtCell* cell)
{
    PreJetInitializer initializer(*this);
    initializer(*cell); //test, try to initialize as we add
    mPreJets.push_back( *cell );
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

//void StConeJetFinder::doMinimization(StJetEtCell& workCell) 
void StConeJetFinder::doMinimization()
{
    mSearchCounter=0;
	
    //enum SearchResult {kTooManyTries, kLeftVolume, kConverged, kContinueSearch};
	
    SearchResult res=kContinueSearch;
    while (res==kContinueSearch) {
		
	//cout <<"\t\t mSearchCounter:\t"<<mSearchCounter<<endl;
	//search in this cone
	res = doSearch();
	
	//cout <<"\t\t\tRESULT:\t"<<res<<endl;
		
	if (res!=kConverged) {
			
	    //find cell corresponding to centroid of cone
	    StEtGridKey key = findKey(mWorkCell.centroid().eta(), mWorkCell.centroid().phi() );
	    StJetEtCell* newCenterCell = findCellByKey(key);
	    if (!newCenterCell) {
		cout <<"newCenterCell doesn't exist.  key:\t"<<key<<endl;
		res=kLeftVolume;
	    }
	    else {
		initializeWorkCell(newCenterCell);
	    }
	}
	else {
	    //add to jet-list
	    addToPrejets(&mWorkCell);
	}
    }
    
    //cout <<"\tSEARCH FINISHED WITH RESULT:\t"<<res<<"\tin:\t"<<mSearchCounter<<"\titerations"<<endl;
}

void StConeJetFinder::findJets(JetList& protojets)
{
  clear();
  fillGrid(protojets);
  setSearchWindow();
  protojets.clear(); //clear 'em, add them back in as we find them
	
  //we partition them so that we don't waste operations on empty cells:
  mTheEnd = std::partition(mVec.begin(), mVec.end(), StJetEtCellIsNotEmpty());
  std::for_each(mVec.begin(), mVec.end(), PreJetUpdater() );
	
  //now we sort them in descending order in et: (et1>et2>...>etn)
  //  std::sort(mVec.begin(), mTheEnd, StJetEtCellEtGreaterThan() ); //This is ok, sorts by lcp-pt here
  mVec.sort(StJetEtCellEtGreaterThan());

  if (mPars.mDebug ) {print();}
	
  //loop from highest et cell to lowest et cell.
  // Begin search over seeds 
  for (CellList::iterator vecIt = mVec.begin(); vecIt != mTheEnd; ++vecIt) {
    
    StJetEtCell* centerCell = *vecIt;
    if (centerCell->eT() <= mPars.mSeedEtMin) break; //we're all done
		
    if (acceptSeed(centerCell)) {
      //new Seed
			
      //use a work object: mWorkCell
      initializeWorkCell(centerCell);
			
      if (mPars.mDoMinimization) {
	doMinimization();
      }
      else {
	doSearch();
	addToPrejets(&mWorkCell);
      }
    }
  }
    
  if (mPars.mAddMidpoints) { 	//add seeds at midpoints
    addSeedsAtMidpoint(); //old style, add midpoints before split/merge
  }
    
  if (mPars.mDoSplitMerge) {//split-merge
    mMerger->splitMerge(mPreJets);
  }
    
  for (ValueCellList::iterator realJetIt=mPreJets.begin(); realJetIt!=mPreJets.end(); ++realJetIt) {
    StJetEtCell* rj = &(*realJetIt);
    if ( rj->cellList().size()>0 ) { //at least one non-empty cell in cone
      protojets.push_back( collectCell(rj) );
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
		if ( (*pj1).distance(*pj2) <= 2.* mPars.mR ) { 
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
	    if (mPars.mRequireStableMidpoints==true) {
		if (res==kConverged) {
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

StJetEtCell* StConeJetFinder::makeCell(double etaMin, double etaMax, double phiMin, double phiMax)
{
    return new StJetEtCell(etaMin, etaMax, phiMin, phiMax);
}

void StConeJetFinder::buildGrid()
{
  double dEta = mPars.mEtaMax - mPars.mEtaMin;
  double dPhi = mPars.mPhiMax - mPars.mPhiMin;
  double etaBinWidth = dEta/static_cast<double>(mPars.mNeta);
  double phiBinWidth = dPhi/static_cast<double>(mPars.mNphi);
	
  for(int i = 0; i < mPars.mNeta; ++i){
		
    double etaMin = mPars.mEtaMin + static_cast<double>(i)*etaBinWidth;
    double etaMax = etaMin + etaBinWidth;
		
    for(int j = 0; j < mPars.mNphi; ++j){
			
      double phiMin = mPars.mPhiMin + static_cast<double>(j)*phiBinWidth;
      double phiMax = phiMin + phiBinWidth;

      StJetEtCell* cell = makeCell(etaMin, etaMax, phiMin, phiMax);
			
      //add it to the vector for ownership and et sorting
      mVec.push_back(cell);
			
      //now add it to the map for cluster finding
      double eta = cell->eta();
      double phi = cell->phi();
			
      StEtGridKey key = findKey(eta, phi);
      CellMapValType val(key, cell);
			
      mMap.insert(val);
    }
  }
}

bool StConeJetFinder::inVolume(double eta, double phi)
{
    int iEta = findEtaKey(eta);
    int iPhi = findPhiKey(phi);
    if (iEta<0 || iPhi<0) {
	return false;
    }
    //const StJetEtCell* cell = findCellByKey( StEtGridKey(iEta, iPhi) );
    return (findCellByKey( StEtGridKey(iEta, iPhi) )) ? true : false;
}

StEtGridKey StConeJetFinder::findKey(double eta, double phi) const
{
    int iEta = findEtaKey(eta);
    int iPhi = findPhiKey(phi);
    if (iEta<0 || iPhi<0) {
	cout <<"StConeJetFinder::findKey(double, double). ERROR:\t"
	     <<"eta:\t"<<eta<<"\tphi:\t"<<phi<<"\t"
	     <<"iEta<0|| iPhi<0\tabort()"<<endl;
	//abort();
    }
    return StEtGridKey(iEta, iPhi);
}

bool StConeJetFinder::acceptSeed(const StJetEtCell* cell)
{
    //return (cell->nTimesUsed()==0 && cell->empty()==false);
    return (cell->empty()==false);
}

bool StConeJetFinder::acceptPair(const StJetEtCell* centerCell, const StJetEtCell* otherCell) const
    //bool StConeJetFinder::acceptPair(StJetEtCell* centerCell, StJetEtCell* otherCell)
{
    return  (
		
	     //both valid
	     centerCell && otherCell 
		
	     //don't add to self
	     //&& ( centerCell->eta()!=otherCell->eta() || centerCell->phi()!=otherCell->phi() )
	     //&& otherCell!=centerCell
		
	     //allow non-unique assignment?
	     //&& otherCell->nTimesUsed()==0
		
	     //no noise
	     && otherCell->empty()==false 
		
	     //cut on associated eT
	     && otherCell->eT()>mPars.mAssocEtMin
		
	     //within cone?
	     && centerCell->distance(*otherCell)<mPars.mR 
		
	     );
}


void StConeJetFinder::setSearchWindow()
{
    mPars.mphiWidth = (mPars.mPhiMax-mPars.mPhiMin)/static_cast<double>(mPars.mNphi);
    mPars.metaWidth = (mPars.mEtaMax-mPars.mEtaMin)/static_cast<double>(mPars.mNeta);
    mPars.mdeltaPhi = static_cast<int>(floor( mPars.mR / mPars.mphiWidth)) + 1;
    mPars.mdeltaEta = static_cast<int>(floor( mPars.mR / mPars.metaWidth)) + 1;
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
    CellMap::iterator where = mMap.find(key);
    return (where!=mMap.end()) ? (*where).second : 0;
}

void StConeJetFinder::fillGrid(JetList& protojets)
{
  for (JetList::iterator protoJet = protojets.begin(); protoJet != protojets.end(); ++protoJet) {
    CellMap::iterator where = mMap.find(findKey((*protoJet).eta(), (*protoJet).phi()));
    if (where != mMap.end())
      (*where).second->add(*protoJet);
    else
      cout << "StConeJetFinder::fillGrid(). ERROR:\t" <<"Could not fill jet in grid."<< endl << *protoJet <<endl;
  }
}

struct StJetEtCellClearer { void operator()(StJetEtCell* lhs) { lhs->clear(); } };

void StConeJetFinder::clear()
{
  mPreJets.clear();
  mSearchCounter = 0;

  for_each(mVec.begin(), mTheEnd, StJetEtCellClearer());
}

void StConeJetFinder::print()
{
    cout <<"\nStConeJetFinder::print()"<<endl;
    cout <<"mNeta:\t"<<mPars.mNeta<<endl;
    cout <<"mNphi:\t"<<mPars.mNphi<<endl;
    cout <<"mEtaMin:\t"<<mPars.mEtaMin<<endl;
    cout <<"mEtaMax:\t"<<mPars.mEtaMax<<endl;    
    cout <<"mPhiMin:\t"<<mPars.mPhiMin<<endl;
    cout <<"mPhiMax:\t"<<mPars.mPhiMax<<endl;
    cout <<"mR:\t"<<mPars.mR<<endl;
    cout <<"mAssocEtMin:\t"<<mPars.mAssocEtMin<<endl;
    cout <<"mSeedEtMin:\t"<<mPars.mSeedEtMin<<endl;
    cout <<"mphiWidth:\t"<<mPars.mphiWidth<<endl;
    cout <<"metaWidth:\t"<<mPars.metaWidth<<endl;
    cout <<"mdeltaPhi:\t"<<mPars.mdeltaPhi<<endl;
    cout <<"mdeltaEta:\t"<<mPars.mdeltaEta<<endl;
    cout <<"mDoMinimization:\t"<<mPars.mDoMinimization<<endl;
    cout <<"mAddMidpoints:\t"<<mPars.mAddMidpoints<<endl;
    cout <<"mDoSplitMerge:\t"<<mPars.mDoSplitMerge<<endl;
    cout <<"mSplitFraction:\t"<<mPars.mSplitFraction<<endl;
    cout <<"splitFraction():\t"<<mMerger->splitFraction()<<endl;
    cout <<"mRequireStableMidpoints:\t"<<mPars.mRequireStableMidpoints<<endl;
    cout <<"mDebug:\t"<<mPars.mDebug<<endl;
}

StConeJetFinder::CellMap::iterator 
StConeJetFinder::findIterator(double eta, double phi)
{
    StEtGridKey centerKey = findKey( eta, phi );
    return findIterator(centerKey);
}


StConeJetFinder::CellMap::iterator StConeJetFinder::findIterator(const StEtGridKey& centerKey)
{
    CellMap::iterator where = mMap.find(centerKey);
    if (where==mMap.end() ) {
	cout <<"StConeJetFinder::findIterator(). ERROR:\t"
	     <<"Mismatch in containers.  Calling abort"<<endl;
	abort();
    }
    return where;
}

//non members ---

void PreJetUpdater ::operator()(StJetEtCell* cell) 
{
    cell->protoJet().update();
    cell->mEt = cell->protoJet().eT();
    sumEt += cell->eT();
}

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
	
    //now update each cell:
    PreJetUpdater updater;
    updater = for_each( cell.cellList().begin(), cell.cellList().end(), updater );
	
    //now update jet-eT
    cell.mEt = updater.sumEt;
}

