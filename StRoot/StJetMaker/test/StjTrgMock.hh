// -*- mode: c++;-*-
// $Id: StjTrgMock.hh,v 1.4 2008/11/03 23:07:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGMOCK_H
#define STJTRGMOCK_H

#include "StjTrg.h"

class StjTrgMock : public StjTrg {

public:
  StjTrgMock() { }
  virtual ~StjTrgMock() { }

  int id() { return _id; }

  int runNumber() { return _runNumber; }
  int eventId() { return _eventId; }
  bool hard() const { return _hard; }
  bool soft() const { return _soft; }
  bool passed() const { return _passed; }
  double prescale() { return _prescale; }
  double vertexZ() { return _vertexZ; }

  std::vector<int> towers()            { return _towers; }
  std::vector<int> towerDsmAdc()       { return _towerDsmAdc; }
  std::vector<unsigned int> towerAdc() { return _towerAdc; }
  std::vector<double> towerEnergy()    { return _towerEnergy; }
  std::vector<double> towerEt()        { return _towerEt; }


  std::vector<int> jetPatches()           { return _jetPatches; }
  std::vector<int> jetPatchDsmAdc()       { return _jetPatchDsmAdc; } 
  std::vector<unsigned int> jetPatchAdc() { return _jetPatchAdc; }
  std::vector<double> jetPatchEnergy()    { return _jetPatchEnergy; } 
  std::vector<double> jetPatchEt()        { return _jetPatchEt; }

  int _id;

  int _runNumber;
  int _eventId;
  bool _hard;
  bool _soft;
  bool _passed;
  double _prescale;
  double _vertexZ;

  std::vector<int>          _towers;
  std::vector<int>          _towerDsmAdc;
  std::vector<unsigned int> _towerAdc;
  std::vector<double>       _towerEnergy;
  std::vector<double>       _towerEt;

  std::vector<int> _jetPatches;
  std::vector<int> _jetPatchDsmAdc;
  std::vector<unsigned int> _jetPatchAdc;
  std::vector<double> _jetPatchEnergy;
  std::vector<double> _jetPatchEt;

private:

  ClassDef(StjTrgMock, 1)

};

#endif // STJTRGMOCK_H
