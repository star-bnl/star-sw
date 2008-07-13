// -*- mode: c++;-*-
// $Id: StJetBEMCEnergyCut.h,v 1.5 2008/07/13 02:38:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCENERGYCUT_H
#define STJETBEMCENERGYCUT_H

#include "TowerEnergyList.h"

#include <set>
#include <iostream>

namespace StJetTowerEnergyCut {

class TowerEnergyCut {

public:
  TowerEnergyCut() { }
  virtual ~TowerEnergyCut() { }

  virtual bool operator()(const StSpinJet::TowerEnergy& deposit) = 0;

private:

};

class TowerEnergyCut2003BemcTower : public TowerEnergyCut {

public:
  TowerEnergyCut2003BemcTower() { }
  virtual ~TowerEnergyCut2003BemcTower() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    const static int badWestTowerId[ ] = { 511, 555, 615, 656, 772, 953, 1042, 1043, 1044, 1045, 1046, 1048, 1385, 1386, 1387, 1408, 1418, 1419, 1555, 1614, 1615, 1616, 1636, 1705, 1706, 1707, 1708, 1725, 1726, 1727, 1728, 1745, 1746, 1747, 1748, 1750, 1765, 1766, 1767, 1768, 1773, 1785, 1786, 1787, 1788, 1866, 1867, 1868, 1869, 1870, 1871, 1872, 1873, 1874, 1875, 1876, 1877, 1878, 1879, 1880, 1881, 1882, 1883, 1884, 1885, 1886, 1887, 1888, 1889, 1890, 1891, 1892, 1893, 1894, 1899, 2073, 2093, 2096, 2127 };
    const static std::set<int> badWestTowerIdSet(badWestTowerId, badWestTowerId + sizeof(badWestTowerId)/sizeof(int));

    if(tower.detectorId != 9) return true;

    if(tower.towerId > 2400) return true;

    if(badWestTowerIdSet.count(tower.towerId)) return true;

    return false;
  }

private:
  
};

class TowerEnergyCutBemcWestOnly : public TowerEnergyCut {

public:
  TowerEnergyCutBemcWestOnly() { }
  virtual ~TowerEnergyCutBemcWestOnly() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.detectorId != 9) return true;

    if(tower.towerId > 2400) return true;

    return false;
  }

private:
  
};

class TowerEnergyCutEnergy : public TowerEnergyCut {

public:
  TowerEnergyCutEnergy(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~TowerEnergyCutEnergy() { }

  bool operator()(const StSpinJet::TowerEnergy& deposit)
  {
    if(deposit.energy <= _min) return true;

    if(deposit.energy > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

class TowerEnergyCutBemcStatus : public TowerEnergyCut {

public:
  TowerEnergyCutBemcStatus(int goodStatus = 1)
    : _goodStatus(goodStatus) { }
  virtual ~TowerEnergyCutBemcStatus() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.status != _goodStatus) return true;

    return false;
  }

private:

  int _goodStatus;
};

class TowerEnergyCutAdc : public TowerEnergyCut {

public:
  TowerEnergyCutAdc(int min = 0, double factor = 2.0)
    : _min(min), _factor(factor) { }
  virtual ~TowerEnergyCutAdc() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.adc - tower.pedestal <= _min) return true;

    if(tower.adc - tower.pedestal <= _factor*tower.rms) return true;

    return false;
  }

private:

  int _min;
  double _factor;
};

}

namespace StSpinJet {

class StJetBEMCEnergyCut {

public:
  StJetBEMCEnergyCut() { }
  virtual ~StJetBEMCEnergyCut() { }
  
  TowerEnergyList Apply(const TowerEnergyList& energyList);

  void addCut(StJetTowerEnergyCut::TowerEnergyCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTowerEnergyCut::TowerEnergyCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const TowerEnergy& deposit);

  CutList _cutList;

};

}

#endif // STJETBEMCENERGYCUT_H
