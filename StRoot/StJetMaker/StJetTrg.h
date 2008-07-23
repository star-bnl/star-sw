// -*- mode: c++;-*-
// $Id: StJetTrg.h,v 1.2 2008/07/23 20:25:42 tai Exp $
#ifndef STJETTRG_H
#define STJETTRG_H

#include "StJetTrgSoftware.h"

class StMuDstMaker;

class StJetTrg {

public:
  StJetTrg(StMuDstMaker* uDstMaker, StJetTrgSoftware* soft)
    : _soft(soft), _uDstMaker(uDstMaker)
  { }
  virtual ~StJetTrg() { }

  int runNumber();
  int eventId();
  bool hard(int trgId);
  bool soft(int trgId);
  double prescale(int trgId);
  double vertexZ();
  std::vector<int> towers(int trgId);
  std::vector<int> jetPatches(int trgId);

private:

  StJetTrgSoftware* _soft;

  StMuDstMaker* _uDstMaker;

};


#endif // STJETTRG_H
