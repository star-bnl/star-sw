// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.2 2008/08/02 22:43:43 tai Exp $
#ifndef STJTRG_H
#define STJTRG_H

#include "StjTrgSoftware.h"

class StMuDstMaker;

class StjTrg {

public:
  StjTrg(int trgId, StMuDstMaker* uDstMaker, StjTrgSoftware* soft)
    : _trgId(trgId), _soft(soft), _uDstMaker(uDstMaker)
  { }
  virtual ~StjTrg() { }

  int id() { return _trgId; }

  int runNumber();
  int eventId();
  bool hard();
  bool soft();
  double prescale();
  double vertexZ();
  std::vector<int> towers();
  std::vector<int> jetPatches();

private:

  int _trgId;

  StjTrgSoftware* _soft;

  StMuDstMaker* _uDstMaker;

};


#endif // STJTRG_H
