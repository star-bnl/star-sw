// -*- mode: c++;-*-
// $Id: StjTrgMuDst.h,v 1.1 2008/08/08 21:16:44 tai Exp $
#ifndef STJTRGMUDST_H
#define STJTRGMUDST_H

#include "StjTrg.h"

#include "StjTrgSoftware.h"

class StMuDstMaker;

class StjTrgMuDst : public StjTrg {

public:
  StjTrgMuDst(int trgId, StMuDstMaker* uDstMaker, StjTrgSoftware* soft)
    : _trgId(trgId), _soft(soft), _uDstMaker(uDstMaker)
  { }
  virtual ~StjTrgMuDst() { }

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

  ClassDef(StjTrgMuDst, 1)

};


#endif // STJTRGMUDST_H
