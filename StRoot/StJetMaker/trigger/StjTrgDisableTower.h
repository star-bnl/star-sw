// -*- mode: c++;-*-
// $Id: StjTrgDisableTower.h,v 1.2 2009/04/28 02:37:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGDISABLETOWER_H
#define STJTRGDISABLETOWER_H

#include <StjTrg.h>

#include <set>

class StjTrgDisableTower : public StjTrg {

public:
  StjTrgDisableTower(StjTrg* src, int badTowerId)
    : _src(src)
    , _badTowerIdSet(&badTowerId, &badTowerId + 1)
    , _runNumber(-1), _eventId(-1) { }
  StjTrgDisableTower(StjTrg* src, int nbadTowerIds, int* badTowerIds)
    : _src(src)
    , _badTowerIdSet(badTowerIds, badTowerIds + nbadTowerIds)
    , _runNumber(-1), _eventId(-1) { }
virtual ~StjTrgDisableTower() { }

  int id() { return _src->id(); }

  int    runNumber()  { return _src->runNumber(); }
  int    eventId()    { return _src->eventId(); }
  bool   hard() const { return _src->hard(); }
  bool   soft() const { return _src->soft(); }
  bool   passed() const { return _src->passed(); }
  double prescale()   { return _src->prescale(); }
  double vertexZ()    { return _src->vertexZ(); }

  virtual std::vector<int>          towers() { return std::vector<int>(); }
  virtual std::vector<int>          towerDsmAdc() { return std::vector<int>(); }
  virtual std::vector<unsigned int> towerAdc() { return std::vector<unsigned int>(); }
  virtual std::vector<double>       towerEnergy() { return std::vector<double>(); }
  virtual std::vector<double>       towerEt() { return std::vector<double>(); }

  virtual std::vector<int>          jetPatches() { return std::vector<int>(); }
  virtual std::vector<int>          jetPatchDsmAdc() { return std::vector<int>(); }
  virtual std::vector<unsigned int> jetPatchAdc() { return std::vector<unsigned int>(); }
  virtual std::vector<double>       jetPatchEnergy() { return std::vector<double>(); }
  virtual std::vector<double>       jetPatchEt() { return std::vector<double>(); }


protected:

  StjTrg* _src;

  std::set<int> _badTowerIdSet;

  void readIfNewEvent() const;

private:

  bool isNewEvent() const;
  virtual void read() const = 0;

  void readNewEvent() const;

  mutable int _runNumber;
  mutable int _eventId;


  ClassDef(StjTrgDisableTower, 1)

};

#endif // STJTRGDISABLETOWER_H
