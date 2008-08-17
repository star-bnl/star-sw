// -*- mode: c++;-*-
// $Id: StjTrgMuDst.h,v 1.2 2008/08/17 11:29:15 tai Exp $
#ifndef STJTRGMUDST_H
#define STJTRGMUDST_H

#include "StjTrg.h"

#include "StjBEMCMuDst.h"

class StjTrgMuDstSoftware;
class StjTrgPassCondition;
class StjTrgBEMCJetPatchTowerIdMap;
class StMuDstMaker;

class StjTrgMuDst : public StjTrg {

public:
  StjTrgMuDst(int trgId, StjTrgPassCondition* passCondition, StMuDstMaker* uDstMaker, StjTrgMuDstSoftware* soft, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap)
    : _trgId(trgId)
    , _passCondition(passCondition)
    , _soft(soft)
    , _uDstMaker(uDstMaker)
    , _bemc(new StjBEMCMuDst(uDstMaker, true))
    , _bemcJpTowerMap(bemcJpTowerMap)
 { }
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

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();

private:

  bool isThereTowerEnergyFor(int towerId);
  StjTowerEnergy findTowerEnergyFor(int towerId);

  int _trgId;

  StjTrgPassCondition* _passCondition;

  StjTrgMuDstSoftware* _soft;

  StMuDstMaker* _uDstMaker;

  StjBEMC* _bemc;

  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  ClassDef(StjTrgMuDst, 1)

};


#endif // STJTRGMUDST_H
