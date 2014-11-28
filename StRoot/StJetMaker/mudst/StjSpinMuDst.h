// -*- mode: c++;-*-
// $Id: StjSpinMuDst.h,v 1.1 2008/11/05 05:48:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJSPINMUDST_H
#define STJSPINMUDST_H

#include <StjSpin.h>

class StMuDstMaker;

class StjSpinMuDst : public StjSpin {

public:
  StjSpinMuDst(StMuDstMaker* uDstMaker)
    : _uDstMaker(uDstMaker) { }
  virtual ~StjSpinMuDst() { }

  int runNumber();
  int eventId();
  int bx7();
  int bx48();
  int spin4();
  int bbcTimebin();
  double vertexZ();

private:

  StMuDstMaker* _uDstMaker;

  ClassDef(StjSpinMuDst, 1)

};

#endif // STJSPINMUDST_H
