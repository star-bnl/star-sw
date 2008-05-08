// $Id: StCdfChargedConeJetFinder.cxx,v 1.27 2008/05/08 05:02:13 tai Exp $
#include "StCdfChargedConeJetFinder.h"

#include "StJetEtCell.h"
#include "StProtoJet.h"
#include "StCdfChargedJetEtCell.h"

StCdfChargedConeJetFinder::StCdfChargedConeJetFinder(const StCdfChargedConePars& pars) 
  : StConeJetFinderBase(pars)
{

}

StCdfChargedConeJetFinder::~StCdfChargedConeJetFinder()
{
}

StJetEtCellFactory* StCdfChargedConeJetFinder::makeCellFactory()
{
  return new StCdfChargedJetEtCellFactory;
}

void StCdfChargedConeJetFinder::findJets(JetList& protoJetList, const FourVecList& particleList)
{
  protoJetList.clear();
  
  for(FourVecList::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
    protoJetList.push_back(StProtoJet(*particle));
  }

  clearPreviousResult();

  CellList orderedList = generateLeadingPtOrderedList(protoJetList);

  CellList toSearchList = generateToSearchListFrom(orderedList);

  findProtoJets(toSearchList);

  storeTheResultIn(protoJetList);

}

void StCdfChargedConeJetFinder::clearPreviousResult()
{
  for(CellList::iterator it = _preJets.begin(); it != _preJets.end(); ++it) {
    delete *it;
  }
  _preJets.clear();
}

void StCdfChargedConeJetFinder::storeTheResultIn(JetList& protoJetList)
{
  protoJetList.clear();
  
  for (CellList::iterator jet = _preJets.begin(); jet != _preJets.end(); ++jet) {

    if ((*jet)->cellList().size() == 0) continue;

    protoJetList.push_back( collectCell(*jet) );
  }
}

void StCdfChargedConeJetFinder::findProtoJets(CellList& toSearchList)
{
    for (CellList::iterator cell = toSearchList.begin(); cell != toSearchList.end(); ++cell) {
  
      if (shouldNotSearchForJetAroundThis((*cell))) continue;

      findJetAroundThis(*cell);
    }
}

StEtaPhiCell::CellList StCdfChargedConeJetFinder::generateLeadingPtOrderedList(JetList& protoJetList)
{
  _cellGrid.fillGridWith(protoJetList);
  return _cellGrid.EtSortedCellList();
}

void StCdfChargedConeJetFinder::findJetAroundThis(StEtaPhiCell* cell)
{
  initializeWorkCell(cell);
  
  formCone();
  addToPrejets(*mWorkCell);
}

bool StCdfChargedConeJetFinder::shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const
{
  if (cell->nTimesUsed()) return true;

  if (cell->empty()) return true;

  return false;
}

bool StCdfChargedConeJetFinder::shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const
{
  if (otherCell.nTimesUsed()) return true;
  if (otherCell.empty()) return true;
  if (otherCell.eT() <= mPars.assocEtMin()) return true; 
  return false;
}

void StCdfChargedConeJetFinder::addToPrejets(StEtaPhiCell& cell)
{
  cell.setEt(0);
  for(CellList::iterator etCell = cell.cellList().begin(); etCell != cell.cellList().end(); ++etCell) {
    (*etCell)->update();
    cell.setEt(cell.Et() + (*etCell)->eT());
  }

  _preJets.push_back(cell.clone());

}

