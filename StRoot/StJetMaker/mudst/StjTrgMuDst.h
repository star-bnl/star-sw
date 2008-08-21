// -*- mode: c++;-*-
// $Id: StjTrgMuDst.h,v 1.5 2008/08/21 22:22:41 tai Exp $
#ifndef STJTRGMUDST_H
#define STJTRGMUDST_H

#include "StjTrg.h"

class StjTrgSoft;
class StjTrgPassCondition;
class StMuDstMaker;

class StjTrgMuDst : public StjTrg {

public:
  StjTrgMuDst(int trgId, StjTrgPassCondition* passCondition, StMuDstMaker* uDstMaker, StjTrgSoft* soft);
  virtual ~StjTrgMuDst() { }

  int id() { return _trgId; }

  int runNumber();
  int eventId();
  bool hard() const;
  bool soft() const;
  bool passed();
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

  StjTrgSoft* _soft;

  StMuDstMaker* _uDstMaker;

  ClassDef(StjTrgMuDst, 1)

};


#endif // STJTRGMUDST_H
