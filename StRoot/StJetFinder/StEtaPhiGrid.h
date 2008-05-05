// -*- mode: c++;-*-
// $Id: StEtaPhiGrid.h,v 1.4 2008/05/05 00:32:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STETAPHIGRID_H
#define STETAPHIGRID_H

#include "StEtGridKey.h"
#include "StEtaPhiCell.h"

#include <map>
#include <list>

class StJetEtCellFactory;
class StConePars;

namespace StSpinJet {

class StEtaPhiGrid {

public:

  typedef std::list<StProtoJet> JetList;
  typedef std::map<StEtGridKey, StEtaPhiCell*> CellMap;
  typedef CellMap::value_type CellMapValType;
  typedef StEtaPhiCell::CellList CellList;
  typedef std::list<StEtaPhiCell> ValueCellList;

  StEtaPhiGrid(StConePars& pars) : _pars(pars) { }

  void buildGrid(StJetEtCellFactory* cellFactory);

  void fillGridWith(JetList& protoJetList);

  CellList EtSortedCellList();
  CellList WithinTheConeRadiusCellList(const StEtaPhiCell& theCell) const;

  StEtaPhiCell* Cell(double eta, double phi);

private:

  StEtaPhiCell* CellI(int iEta, int iPhi) const;

  StEtGridKey findKey(double eta, double phi) const;
  int findEtaKey(double eta) const;
  int findPhiKey(double phi) const;

  StConePars& _pars;
  CellMap _EtCellMap;
  CellList _EtCellList;

};

}

#endif // STETAPHIGRID_H
