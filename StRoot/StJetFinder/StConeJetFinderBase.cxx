// $Id: StConeJetFinderBase.cxx,v 1.11 2010/07/02 21:47:56 pibero Exp $
#include "StConeJetFinderBase.h"

#include "TObject.h"

//std
#include <iostream>
#include <algorithm>
#include <time.h>
#include <map>
using std::sort;

//StJetFinder
#include "StJetEtCell.h"
#include "StJetSpliterMerger.h"
#include "StProtoJet.h"

using namespace StSpinJet;

StConeJetFinderBase::StConeJetFinderBase(const StConePars& pars)
  : mPars(pars)
  , mWorkCell(new StJetEtCell)
  , _cellGrid(mPars)
{

}

StConeJetFinderBase::~StConeJetFinderBase()
{
  delete mWorkCell;
}

void StConeJetFinderBase::Init()
{
  _cellGrid.buildGrid(makeCellFactory());
}

StEtaPhiCell::CellList StConeJetFinderBase::generateToSearchListFrom(CellList& orderedList)
{
  CellList toSearchList;
 
  for (CellList::iterator cell = orderedList.begin(); cell != orderedList.end(); ++cell) {
  
    if ((*cell)->eT() <= mPars.seedEtMin()) break;
  
    toSearchList.push_back(*cell);

  }
 
  return toSearchList;
}

void StConeJetFinderBase::initializeWorkCell(const StEtaPhiCell* other)
{
    mWorkCell->clear();
    *mWorkCell = *other;
    mWorkCell->setEt(0.);
    if (mWorkCell->cellList().empty()==false) {
	cout <<"StConeJetFinderBase::initializeWorkCell(). ERROR:\t"
	     <<"workCell is not empty. abort()"<<endl;
	abort();
    }
}

void StConeJetFinderBase::formCone()
{
  CellList cellList = _cellGrid.WithinTheConeRadiusCellList(*mWorkCell);

  for (CellList::iterator cell = cellList.begin(); cell != cellList.end(); ++cell) {
    if(shouldNotAddToTheCell(*mWorkCell, **cell)) continue;
    mWorkCell->addCell(*cell);
  }
}

const StProtoJet& StConeJetFinderBase::collectCell(StEtaPhiCell* seed)
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
	cout <<"\tStConeJetFinderBase::collectCell(). ERROR:\t"
	     <<"null cell.  skip"<<endl;
      }
      if (centerCell==*it) {
	cout <<"\tStConeJetFinderBase::collectCell(). ERROR:\t"
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

