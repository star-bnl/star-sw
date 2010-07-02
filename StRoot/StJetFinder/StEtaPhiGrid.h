// -*- mode: c++;-*-
// $Id: StEtaPhiGrid.h,v 1.7 2010/07/02 21:47:56 pibero Exp $
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

  StEtaPhiGrid(StConePars& pars) : _pars(pars) { }

  ~StEtaPhiGrid();

  void buildGrid(StJetEtCellFactory* cellFactory);

  void fillGridWith(JetList& protoJetList);

  CellList EtSortedCellList();
  CellList WithinTheConeRadiusCellList(const StEtaPhiCell& theCell) const;

  StEtaPhiCell* findMidpointCell(const StEtaPhiCell& cell1, const StEtaPhiCell& cell2);

  StEtaPhiCell* Cell(double eta, double phi);

private:

  StEtaPhiCell* CellI(int iEta, int iPhi) const;

  StEtGridKey findKey(double eta, double phi) const;
  int findEtaKey(double eta) const;
  int findPhiKey(double phi) const;

  double midpoint(double v1, double v2);

  StConePars& _pars;
  CellMap _EtCellMap;
  CellList _EtCellList;

};

}

#endif // STETAPHIGRID_H
