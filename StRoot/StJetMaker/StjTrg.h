// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.1 2008/08/02 04:06:19 tai Exp $
#ifndef STJETTRG_H
#define STJETTRG_H

#include "StjTrgSoftware.h"

class StMuDstMaker;

class StJetTrg {

public:
  StJetTrg(int trgId, StMuDstMaker* uDstMaker, StJetTrgSoftware* soft)
    : _trgId(trgId), _soft(soft), _uDstMaker(uDstMaker)
  { }
  virtual ~StJetTrg() { }

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

  StJetTrgSoftware* _soft;

  StMuDstMaker* _uDstMaker;

};


#endif // STJETTRG_H
