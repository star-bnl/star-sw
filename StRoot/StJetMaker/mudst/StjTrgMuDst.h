// -*- mode: c++;-*-
// $Id: StjTrgMuDst.h,v 1.1 2008/08/08 23:18:49 tai Exp $
#ifndef STJTRGMUDST_H
#define STJTRGMUDST_H

#include "StjTrg.h"

class StjTrgMuDstSoftware;
class StjTrgPassCondition;
class StMuDstMaker;

class StjTrgMuDst : public StjTrg {

public:
  StjTrgMuDst(int trgId, StjTrgPassCondition* passCondition, StMuDstMaker* uDstMaker, StjTrgMuDstSoftware* soft)
    : _trgId(trgId), _passCondition(passCondition), _soft(soft), _uDstMaker(uDstMaker) { }
  virtual ~StjTrgMuDst() { }

  int id() { return _trgId; }

  int runNumber();
  int eventId();
  bool hard() const;
  bool soft() const;
  bool pass();
  double prescale();
  double vertexZ();
  std::vector<int> towers();
  std::vector<int> jetPatches();

private:

  int _trgId;

  StjTrgPassCondition* _passCondition;

  StjTrgMuDstSoftware* _soft;

  StMuDstMaker* _uDstMaker;


  ClassDef(StjTrgMuDst, 1)

};


#endif // STJTRGMUDST_H
