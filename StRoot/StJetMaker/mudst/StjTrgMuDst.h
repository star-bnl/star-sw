// -*- mode: c++;-*-
// $Id: StjTrgMuDst.h,v 1.3 2008/08/18 06:20:45 tai Exp $
#ifndef STJTRGMUDST_H
#define STJTRGMUDST_H

#include "StjTrg.h"

class StjTrgMuDstSoftware;
class StjTrgPassCondition;
class StMuDstMaker;

class StjTrgMuDst : public StjTrg {

public:
  StjTrgMuDst(int trgId, StjTrgPassCondition* passCondition, StMuDstMaker* uDstMaker, StjTrgMuDstSoftware* soft);
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
  std::vector<int> towerDsmAdc();
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();
  std::vector<double> jetPatchEt();

private:

  int _trgId;

  StjTrgPassCondition* _passCondition;

  StjTrgMuDstSoftware* _soft;

  StMuDstMaker* _uDstMaker;

  ClassDef(StjTrgMuDst, 1)

};


#endif // STJTRGMUDST_H
