// -*- mode: c++;-*-
// $Id: StjMCKinMuDst.h,v 1.1 2008/08/22 22:10:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCKINMUDST_H
#define STJMCKINMUDST_H

#include <StjMCKin.h>

class StMuDstMaker;

class StjMCKinMuDst : public StjMCKin {

public:
  StjMCKinMuDst(StMuDstMaker* uDstMaker)
    : _uDstMaker(uDstMaker), _runNumber(-1), _eventId(-1) { }
  virtual ~StjMCKinMuDst() { }

  int runNumber();
  int eventId();
  double vertexZ();
  double s();
  double t();
  double u();
  double pt();
  double costh();
  double x1();
  double x2();
  int pid();

private:

  void readIfNewEvent() const;
  bool isNewEvent() const;
  void readNewEvent() const;

  StMuDstMaker* _uDstMaker;

  mutable int _runNumber;
  mutable int _eventId;
  mutable double _s;
  mutable double _t;
  mutable double _u;
  mutable double _pt;
  mutable double _costh;
  mutable double _x1;
  mutable double _x2;
  mutable int    _pid;

  mutable double _vertexZ;

  ClassDef(StjMCKinMuDst, 1)

};

#endif // STJMCKINMUDST_H
