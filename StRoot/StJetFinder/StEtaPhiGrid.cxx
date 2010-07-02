// -*- mode: c++;-*-
// $Id: StEtaPhiGrid.cxx,v 1.8 2010/07/02 21:47:56 pibero Exp $
#include "StEtaPhiGrid.h"

#include "StConePars.h"
#include "StJetEtCellFactory.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

StEtaPhiGrid::~StEtaPhiGrid()
{
  for (CellList::iterator cell = _EtCellList.begin(); cell != _EtCellList.end(); ++cell)
    delete *cell;
}

void StEtaPhiGrid::buildGrid(StJetEtCellFactory* cellFactory)
{
  for(int i = 0; i < _pars.Neta(); ++i){
		
    double etaMin = _pars.EtaMin() + static_cast<double>(i)*_pars.etaWidth();
    double etaMax = etaMin + _pars.etaWidth();
		
    for(int j = 0; j < _pars.Nphi(); ++j){
			
      double phiMin = _pars.PhiMin() + static_cast<double>(j)*_pars.phiWidth();
      double phiMax = phiMin + _pars.phiWidth();

      StEtaPhiCell* cell = cellFactory->create(etaMin, etaMax, phiMin, phiMax);
			
      _EtCellList.push_back(cell);
			
      _EtCellMap.insert(CellMapValType(findKey(cell->eta(), cell->phi()), cell));
    }
  }

}

void StEtaPhiGrid::fillGridWith(JetList& protoJetList)
{
  for(CellList::iterator etCell = _EtCellList.begin(); etCell != _EtCellList.end(); ++etCell) {
    (*etCell)->clear();
  }

  for (JetList::iterator protoJet = protoJetList.begin(); protoJet != protoJetList.end(); ++protoJet) {
    CellMap::iterator where = _EtCellMap.find(findKey((*protoJet).eta(), (*protoJet).phi()));
    if (where != _EtCellMap.end())
      (*where).second->addProtoJet(*protoJet);
    //    else
      //      cout << "StEtaPhiGrid::fillGrid(). ERROR:\t" <<"Could not fill jet in grid."<< endl << *protoJet << endl;
  }

  for(CellList::iterator etCell = _EtCellList.begin(); etCell !=  _EtCellList.end(); ++etCell) {
    (*etCell)->update();
  }
}

StEtaPhiGrid::CellList StEtaPhiGrid::EtSortedCellList()
{
  _EtCellList.sort(StJetEtCellEtGreaterThan());
  return _EtCellList;
}

StEtaPhiGrid::CellList StEtaPhiGrid::WithinTheConeRadiusCellList(const StEtaPhiCell& theCell) const
{
  CellList ret;

  StEtGridKey centerKey = findKey(theCell.eta(), theCell.phi());

  int iEtaMin = centerKey.eta() - _pars.deltaEta();
  if (iEtaMin < 0) iEtaMin = 0 ;

  for(int iEta = iEtaMin; (iEta <= centerKey.eta() + _pars.deltaEta()) && (iEta < _pars.Neta()); ++iEta) {
    for (int iPhi = centerKey.phi() - _pars.deltaPhi(); iPhi <= centerKey.phi() + _pars.deltaPhi(); ++iPhi) {

      int iModPhi = iPhi;
      if (iModPhi < 0) iModPhi = iModPhi + _pars.Nphi();
      if (iModPhi >= _pars.Nphi()) iModPhi = iModPhi - _pars.Nphi();

      StEtaPhiCell* otherCell = CellI(iEta, iModPhi);

      if(theCell.distance(*otherCell) >= _pars.coneRadius()) continue; 

      ret.push_back(otherCell);

    }
  }

  return ret;
}

StEtaPhiCell* StEtaPhiGrid::Cell(double eta, double phi)
{
  CellMap::iterator it = _EtCellMap.find(findKey(eta, phi));
  return (it != _EtCellMap.end()) ? (*it).second : 0;
}

StEtaPhiCell* StEtaPhiGrid::CellI(int iEta, int iPhi) const
{
  CellMap::const_iterator it = _EtCellMap.find(StEtGridKey(iEta, iPhi));
  return (it != _EtCellMap.end()) ? (*it).second : 0;
}

StEtGridKey StEtaPhiGrid::findKey(double eta, double phi) const
{
  int iEta = findEtaKey(eta);
  int iPhi = findPhiKey(phi);
  if (iEta < 0 || iPhi < 0) {
    //    cout << "StEtGridKey::findKey(double, double). ERROR:\t"
    //	 << "eta:\t" << eta << "\tphi:\t" << phi << "\t"
    //	 << "iEta<0|| iPhi<0\tabort()" << endl;
  }
  return StEtGridKey(iEta, iPhi);
}


int StEtaPhiGrid::findEtaKey(double eta) const
{
  return int((_pars.Neta()/(_pars.EtaMax() - _pars.EtaMin()))*(eta - _pars.EtaMin()));
}

int StEtaPhiGrid::findPhiKey(double phi) const
{
  while(phi > M_PI) phi -= 2*M_PI;
  while(phi < -M_PI) phi += 2*M_PI;
  return int( _pars.Nphi()*((phi - _pars.PhiMin())/(_pars.PhiMax() - _pars.PhiMin())));
}

double StEtaPhiGrid::midpoint(double v1, double v2)
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

StEtaPhiCell* StEtaPhiGrid::findMidpointCell(const StEtaPhiCell& cell1, const StEtaPhiCell& cell2)
{
  //  return Cell((cell1.eta() + cell2.eta())/2.0, (cell1.phi() + cell2.phi())/2.0);
  return Cell(midpoint(cell1.eta(), cell2.eta()), midpoint(cell1.phi(), cell2.phi()));
}


}
