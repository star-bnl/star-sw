// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.2 2008/08/02 19:22:28 tai Exp $
#ifndef STJETTRG_H
#define STJETTRG_H

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


#endif // STJETTRG_H
