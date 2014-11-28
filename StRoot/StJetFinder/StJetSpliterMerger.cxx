// $Id: StJetSpliterMerger.cxx,v 1.9 2008/05/08 05:02:13 tai Exp $
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

void StJetSpliterMerger::splitMerge(CellList& preJets)
{
  _preJets.clear();
  copy(preJets.begin(), preJets.end(), back_inserter(_preJets));
  preJets.clear();

  while(!_preJets.empty()) {

    _OverlapList.clear();

    _preJets.sort(StJetEtCellEtGreaterThan());

    for (CellList::iterator otherIt = ++(_preJets.begin()); otherIt != _preJets.end(); ++otherIt) {

      StEtaPhiCell::CellList& otherCells = (*otherIt)->cellList();
      EtNeighbor neighbor;

      for (StEtaPhiCell::CellList::iterator rootSetIt = _preJets.front()->cellList().begin(); rootSetIt != _preJets.front()->cellList().end(); ++rootSetIt) {
	for (StEtaPhiCell::CellList::iterator otherSetIt = otherCells.begin(); otherSetIt != otherCells.end(); ++otherSetIt) {
	  neighbor.check(*rootSetIt, *otherSetIt);
	}
      }

      if (neighbor.nCommonCells <= 0) continue;
       
      neighbor._otherCell = otherIt;
      _OverlapList.push_back(neighbor);

    }

    if (_OverlapList.empty()) {
      preJets.push_back(_preJets.front());
      _preJets.pop_front();
    } else {

      EtNeighbor& n = *min_element(_OverlapList.begin(), _OverlapList.end());

      StEtaPhiCell& neighborJet = **(n._otherCell);

      if (n.sharedEt/neighborJet.eT() > splitFraction() ) { //merge these two
	merge(*_preJets.front(), neighborJet, n.cells);

	//remove neighbor jet
	_preJets.erase(n._otherCell);
      } else { //split these two
	split(*_preJets.front(), neighborJet, n.cells);
      }

    }
  }
}

//this really has to be encapsulated in StEtaPhiCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::merge(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells)
{
  CellList& rootList = root.cellList();
  CellList& neighborList = neighbor.cellList();
  
  for (CellList::iterator it2=neighborList.begin(); it2!=neighborList.end(); ++it2) {
    //add to root
    if (std::find(rootList.begin(), rootList.end(), (*it2))==rootList.end() ) {
      rootList.push_back(*it2);		
    }
  }

  //assume that neighbor will be discarded. Don't waste time removing cells from neighbor
  PostMergeUpdater updater;
  updater(root);
}

//this really has to be encapsulated in StEtaPhiCell class.  For now we bust out the guts of the code here
void StJetSpliterMerger::split(StEtaPhiCell& root, StEtaPhiCell& neighbor, CellVec& commonCells)
{
  CellList& rootList = root.cellList();
  CellList& neighborList = neighbor.cellList();

  for (CellVec::iterator it=commonCells.begin(); it!=commonCells.end(); ++it) {
	
    double distanceToRoot = root.distance(**it);
    double distanceToNeighbor = neighbor.distance(**it);
    if (distanceToRoot<distanceToNeighbor) { //root keeps it
      neighborList.remove(*it);
    } else { //neighbor keeps it
      rootList.remove(*it);
    }
  }

  PostMergeUpdater updater;
  updater(root);
  updater(neighbor);

}


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
