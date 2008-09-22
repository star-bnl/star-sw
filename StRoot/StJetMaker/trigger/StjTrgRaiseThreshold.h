// -*- mode: c++;-*-
// $Id: StjTrgRaiseThreshold.h,v 1.4 2008/09/22 00:06:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGRAISETHRESHOLD_H
#define STJTRGRAISETHRESHOLD_H

#include <StjTrg.h>

class StjTrgRaiseThreshold : public StjTrg {

public:
  StjTrgRaiseThreshold(StjTrg* src)
    : _src(src)
    , _runNumber(-1), _eventId(-1) { }
virtual ~StjTrgRaiseThreshold() { }

  int id() { return _src->id(); }

  int    runNumber()  { return _src->runNumber(); }
  int    eventId()    { return _src->eventId(); }
  bool   hard() const { return _src->hard(); }
  virtual bool soft() const = 0;
  bool   passed()  const   { return (_src->passed() && soft()); }
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

  void readIfNewEvent() const;

private:

  bool isNewEvent() const;
  virtual void read() const = 0;

  void readNewEvent() const;

  mutable int _runNumber;
  mutable int _eventId;


  ClassDef(StjTrgRaiseThreshold, 1)

};

#endif // STJTRGRAISETHRESHOLD_H
