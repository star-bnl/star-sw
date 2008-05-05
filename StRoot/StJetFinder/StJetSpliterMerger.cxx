// $Id: StJetSpliterMerger.cxx,v 1.6 2008/05/05 00:32:49 tai Exp $
//StJetSpliterMerger.cxx
//M.L. Miller (Yale Software)
//10/02

//std
#include <iostream>
#include <ctime>
#include <algorithm>
#include <list>
#include <utility>
#include <functional>
#include <vector>
#include <map>
using std::map;
using std::multimap;
using std::vector;
using std::copy;
using std::find;
using std::find_first_of;
using std::sort;

#include "StProtoJet.h"
#include "StEtaPhiCell.h"
#include "Functors.h"
#include "StJetSpliterMerger.h"

void StJetSpliterMerger::splitMerge(ValueCellList& preJets)
{
  clock_t begin = clock(); if(begin) {/*touch*/}
	
  copyPreJets(preJets);
		
  while(!mPreJets.empty()) {

    mOverlapMap.clear();

    //sort them in descending order in et: (et1>et2>...>etn)
    mPreJets.sort(std::greater<StEtaPhiCell>());
	
    ValueCellList::iterator rootIt = mPreJets.begin();
    StEtaPhiCell& root = *rootIt;
    StEtaPhiCell::CellList& rootCells = root.cellList();
		
    // We define a comparison between the "root" jet and the "other" jet.
		
    for (ValueCellList::iterator otherIt = rootIt; otherIt != mPreJets.end(); ++otherIt) {
			
      if (otherIt != rootIt) { //don't compare to self
				
	StEtaPhiCell::CellList& otherCells = (*otherIt).cellList();
	EtNeighbor neighbor;
				
	for (StEtaPhiCell::CellList::iterator rootSetIt = rootCells.begin(); rootSetIt != rootCells.end(); ++rootSetIt) {
	  for (StEtaPhiCell::CellList::iterator otherSetIt = otherCells.begin(); otherSetIt != otherCells.end(); ++otherSetIt) {
	    neighbor.check(*rootSetIt, *otherSetIt);
	  }
	}
	  
	if (neighbor.nCommonCells>0) {
	  neighbor.location = otherIt;
	  EtNeighborMap::value_type myN(neighbor, neighbor);
	  mOverlapMap.insert( myN );
	}
      }
    }

    if (mOverlapMap.empty()) { //this guy shares no eT!
      preJets.push_back(root);
      mPreJets.erase(rootIt);
    } else {
      EtNeighbor& n = (*(mOverlapMap.begin())).second;
      StEtaPhiCell& neighborJet = *(n.location);

      if (n.sharedEt/neighborJet.eT() > splitFraction() ) { //merge these two
	merge(root, neighborJet, n.cells);

	//remove neighbor jet
	mPreJets.erase(n.location);
      } else { //split these two
	split(root, neighborJet, n.cells);
      }
    }
  }
	

}

//this really has to be encapsulated in StEtaPhiCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::merge(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells)
{
    CellList& rootList = root.cellList();
    CellList& neighborList = neighbor.cellList();

    /*
      for (CellVec::iterator it=commonCells.begin(); it!=commonCells.end(); ++it) {
		
      //simple temporary check
      if (std::find(rootList.begin(), rootList.end(), (*it))==rootList.end()
      || std::find(neighborList.begin(), neighborList.end(), (*it))==neighborList.end() ) {
      cout <<"ERROR: common cell not in both lists.  abort"<<endl;
      abort();
      }
      }
    */

    for (CellList::iterator it2=neighborList.begin(); it2!=neighborList.end(); ++it2) {
	//add to root
	if (std::find(rootList.begin(), rootList.end(), (*it2))==rootList.end() ) {
	    rootList.push_back(*it2);		
	}
    }

    //assume that neighbor will be discarded. Don't waste time removing cells from neighbor
    PostMergeUpdater updater;
    updater(root);

    //cout <<"root after merge:    \t"<<root<<endl;
}

//this really has to be encapsulated in StEtaPhiCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::split(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells)
{
    CellList& rootList = root.cellList();
    CellList& neighborList = neighbor.cellList();

    for (CellVec::iterator it=commonCells.begin(); it!=commonCells.end(); ++it) {
	
	/*
	//simple temporary check
	if (std::find(rootList.begin(), rootList.end(), (*it))==rootList.end()
	|| std::find(neighborList.begin(), neighborList.end(), (*it))==neighborList.end() ) {
	cout <<"ERROR: common cell not in both lists.  abort"<<endl;
	abort();
	}
	*/

	double distanceToRoot = root.distance(**it);
	double distanceToNeighbor = neighbor.distance(**it);
	if (distanceToRoot<distanceToNeighbor) { //root keeps it
	    neighborList.remove(*it);
	}
	else { //neighbor keeps it
	    rootList.remove(*it);
	}
    }

    PostMergeUpdater updater;
    updater(root);
    updater(neighbor);

    //cout <<"root after removal:    \t"<<root<<endl;
    //cout <<"neighbor after removal:\t"<<neighbor<<endl;
}

/*
  void StJetSpliterMerger::copyPostJets(ValueCellList& preJets)
  {
  preJets.resize( mPreJets.size() );
  copy(mPreJets.begin(), mPreJets.end(), preJets.begin() );
  }
*/

void StJetSpliterMerger::copyPreJets(ValueCellList& preJets)
{
    mPreJets.clear();
    mPreJets.resize(preJets.size());
    copy(preJets.begin(), preJets.end(), mPreJets.begin() );
    preJets.clear();
}

EtNeighbor::EtNeighbor() 
{
    nCommonCells=0; 
    sharedEt=0.; 
    cells.clear();
}

EtNeighbor::EtNeighbor(ValueCellList::iterator it, int n, double et)
    : location(it), nCommonCells(n), sharedEt(et) 
{
    cells.clear();
}
