// -*- mode: c++;-*-
// $Id: StJetTrg.h,v 1.3 2008/07/24 02:14:48 tai Exp $
#ifndef STJETTRG_H
#define STJETTRG_H

#include "StJetTrgSoftware.h"

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
