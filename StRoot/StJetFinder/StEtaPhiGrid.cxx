// -*- mode: c++;-*-
// $Id: StEtaPhiGrid.cxx,v 1.3 2008/04/30 00:23:34 tai Exp $
#include "StEtaPhiGrid.h"

#include "StConePars.h"
#include "StJetEtCellFactory.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StEtaPhiGrid::buildGrid(StJetEtCellFactory* cellFactory)
{
  for(int i = 0; i < _pars.Neta(); ++i){
		
    double etaMin = _pars.EtaMin() + static_cast<double>(i)*_pars.etaWidth();
    double etaMax = etaMin + _pars.etaWidth();
		
    for(int j = 0; j < _pars.Nphi(); ++j){
			
      double phiMin = _pars.PhiMin() + static_cast<double>(j)*_pars.phiWidth();
      double phiMax = phiMin + _pars.phiWidth();

      StJetEtCell* cell = cellFactory->create(etaMin, etaMax, phiMin, phiMax);
			
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
      (*where).second->add(*protoJet);
    else
      cout << "StEtaPhiGrid::fillGrid(). ERROR:\t" <<"Could not fill jet in grid."<< endl << *protoJet << endl;
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

StEtaPhiGrid::CellList StEtaPhiGrid::WithinTheConeRadiusCellList(const StJetEtCell& theCell) const
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

      StJetEtCell* otherCell = CellI(iEta, iModPhi);

      if(theCell.distance(*otherCell) >= _pars.coneRadius()) continue; 

      ret.push_back(otherCell);

    }
  }

  return ret;
}

StJetEtCell* StEtaPhiGrid::Cell(double eta, double phi)
{
  CellMap::iterator it = _EtCellMap.find(findKey(eta, phi));
  return (it != _EtCellMap.end()) ? (*it).second : 0;
}

StJetEtCell* StEtaPhiGrid::CellI(int iEta, int iPhi) const
{
  CellMap::const_iterator it = _EtCellMap.find(StEtGridKey(iEta, iPhi));
  return (it != _EtCellMap.end()) ? (*it).second : 0;
}

StEtGridKey StEtaPhiGrid::findKey(double eta, double phi) const
{
  int iEta = findEtaKey(eta);
  int iPhi = findPhiKey(phi);
  if (iEta < 0 || iPhi < 0) {
    cout << "StEtGridKey::findKey(double, double). ERROR:\t"
	 << "eta:\t" << eta << "\tphi:\t" << phi << "\t"
	 << "iEta<0|| iPhi<0\tabort()" << endl;
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

}
