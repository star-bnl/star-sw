#ifdef WIN32
#include "stdafx.h"
#endif

//StJetSpliterMerger.cxx
//M.L. Miller (Yale Software)
//10/02

//std
#include <Stiostream.h>
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
#include "StJetEtCell.h"
#include "Functors.h"
#include "StJetSpliterMerger.h"

void StJetSpliterMerger::splitMerge(ValueCellList& preJets)
{
    //cout <<"--- StJetSpliterMerger::splitMerge(CellList&) ---"<<endl;
    clock_t begin = clock(); if(begin) {/*touch*/}
	
    //copy into work array
    copyPreJets(preJets);

    if (0) {
	cout <<"\n------- Pre- Jet List ------------------------------------------ "<<endl;
	//for (ValueCellList::const_iterator it=mPreJets.begin(); it!=mPreJets.end(); ++it) {
	for (ValueCellList::iterator it=mPreJets.begin(); it!=mPreJets.end(); ++it) {
	    cout <<"\n"<<*it<<endl;
	}
    }
		
    //now begin loop
	
    //bool go=true;
    //while (go) { go=false;
    while (mPreJets.empty()==false) {

	mOverlapMap.clear();

	//sort them in descending order in et: (et1>et2>...>etn)
#ifndef __SUNPRO_CC
	mPreJets.sort( 	std::greater<StJetEtCell>());
#else
	cout <<"StJetSpliterMerger::splitMerge(). ERROR:\t"
	     <<"This function not supported under current version of SOLARIS. abort()"<<endl;
	abort();
#endif
	
	if (0) {
	    cout <<"\n------- Pre- Jet List ------------------------------------------ "<<endl;
	    //for (ValueCellList::const_iterator it=mPreJets.begin(); it!=mPreJets.end(); ++it) {
	    for (ValueCellList::iterator it=mPreJets.begin(); it!=mPreJets.end(); ++it) {
		cout <<"\n"<<*it<<endl;
	    }
	}
		
	//for (ValueCellList::iterator rootIt = mPreJets.begin(); rootIt!=mPreJets.end(); ++rootIt) {
		
	ValueCellList::iterator rootIt = mPreJets.begin();
	StJetEtCell& root = *rootIt;
	StJetEtCell::CellList& rootCells = root.cellList();
		
	// We define a comparison between the "root" jet and the "other" jet.
		
	for (ValueCellList::iterator otherIt=rootIt; otherIt!=mPreJets.end(); ++otherIt) {
			
	    if (otherIt!=rootIt) { //don't compare to self
				
		StJetEtCell::CellList& otherCells = (*otherIt).cellList();
		EtNeighbor neighbor;
				
		for (StJetEtCell::CellList::iterator rootSetIt=rootCells.begin(); rootSetIt!=rootCells.end(); ++rootSetIt) {
		    for (StJetEtCell::CellList::iterator otherSetIt=otherCells.begin(); otherSetIt!=otherCells.end(); ++otherSetIt) {
						
			//now test root_set =? other_set
			if ( neighbor.check(*rootSetIt, *otherSetIt)==true) { //this cell common
			    //cout <<"common cell:\t"<<*rootSetIt<<endl;
			}
		    }
		}
				
		if (neighbor.nCommonCells>0) {
		    if (0) { //debug info
			cout <<"\n ------- New Root ------- "<<endl;
			cout <<"--- rootCell:\n"<<*rootIt<<endl;		
			cout <<"--- otherCell:\n"<<*otherIt<<endl;
			cout <<" These jets share:\t"<<neighbor.sharedEt<<"\tGeV over:\t"<<neighbor.nCommonCells<<"\tcells"<<endl;
		    }
		    neighbor.location = otherIt;
		    EtNeighborMap::value_type myN(neighbor, neighbor);
		    mOverlapMap.insert( myN );
		    //mOverlapMap[neighbor]=neighbor;
		}
	    }
	}
	if (0) {
	    cout <<"\t\tContents of map"<<endl;
	    cout <<"\t\t--------------"<<endl;
	    for (EtNeighborMap::const_iterator itt=mOverlapMap.begin(); itt!=mOverlapMap.end(); ++itt) {
		cout <<(*itt).second<<endl;
	    }
	}

	if (mOverlapMap.empty()==true) { //this guy shares no eT!
	    //cout <<"root Jet shares no eT, he's done!"<<endl;
	    //if (root.eT()!=0.) { //don't return empty cells as jets
	    preJets.push_back(root);
	    //}
	    mPreJets.erase(rootIt);
	    //mPreJets.remove(root); //slow, error prone
	}
	else {
	    EtNeighbor& n = (*(mOverlapMap.begin())).second;
	    StJetEtCell& neighborJet = *(n.location);

	    if (n.sharedEt/neighborJet.eT() > splitFraction() ) { //merge these two
		//cout <<"MERGE THESE JETS"<<endl;
		merge(root, neighborJet, n.cells);

		//remove neighbor jet
		mPreJets.erase(n.location);
		//mPreJets.remove(neighborJet); //shit,this removes *all* jets that compare ==
	    }
	    else { //split these two
		//cout <<"SPLIT THESE JETS"<<endl;
		split(root, neighborJet, n.cells);
	    }
	}
	//}
    }
	
    //copyPostJets(preJets);
    //clock_t end = clock();
    //double elapsedTime = static_cast<double>(end-begin)/static_cast<double>(CLOCKS_PER_SEC);
    //cout <<"\tFinished in:\t"<<elapsedTime<<"\tcpu sec"<<endl;

}

//this really has to be encapsulated in StJetEtCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::merge(StJetEtCell& root, StJetEtCell& neighbor, CellVec& commonCells)
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

//this really has to be encapsulated in StJetEtCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::split(StJetEtCell& root, StJetEtCell& neighbor, CellVec& commonCells)
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
