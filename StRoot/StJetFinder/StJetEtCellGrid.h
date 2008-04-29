// -*- mode: c++;-*-
// $Id: StJetEtCellGrid.h,v 1.5 2008/04/29 20:32:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETETCELLGRID_H
#define STJETETCELLGRID_H

#include "StEtGridKey.h"
#include "StJetEtCell.h"

#include <map>
#include <list>

class StJetEtCellFactory;
class StConePars;

namespace StSpinJet {

class StJetEtCellGrid {

public:

  typedef std::list<StProtoJet> JetList;
  typedef std::map<StEtGridKey, StJetEtCell*> CellMap;
  typedef CellMap::value_type CellMapValType;
  typedef StJetEtCell::CellList CellList;
  typedef std::list<StJetEtCell> ValueCellList;

  StJetEtCellGrid(StConePars& pars) : _pars(pars) { }

  void buildGrid(StJetEtCellFactory* cellFactory);

  void fillGridWith(JetList& protoJetList);
  CellList EtSortedCellList();

  StJetEtCell* CellI(int iEta, int iPhi);
  StJetEtCell* CellD(double eta, double phi);

private:

  StEtGridKey findKey(double eta, double phi) const;
  int findEtaKey(double eta) const;
  int findPhiKey(double phi) const;

  StConePars& _pars;
  CellMap _EtCellMap;
  CellList _EtCellList;

};

}

#endif // STJETETCELLGRID_H
