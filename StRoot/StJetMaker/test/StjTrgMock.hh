// -*- mode: c++;-*-
// $Id: StjTrgMock.hh,v 1.1 2008/08/12 04:01:34 tai Exp $
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
  bool pass() { return _pass; }
  double prescale() { return _prescale; }
  double vertexZ() { return _vertexZ; }
  std::vector<int> towers() { return _towers; }
  std::vector<int> jetPatches() { return _jetPatches; }

  int _id;

  int _runNumber;
  int _eventId;
  bool _hard;
  bool _soft;
  bool _pass;
  double _prescale;
  double _vertexZ;
  std::vector<int> _towers;
  std::vector<int> _jetPatches;

private:

  ClassDef(StjTrgMock, 1)

};

#endif // STJTRGMOCK_H
