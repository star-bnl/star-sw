// -*- mode: c++;-*-
// $Id: StjTrgSoftMuDstTriggerSimuMakerFactory.h,v 1.1 2008/08/18 06:37:27 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StjTrgSoftFactory.h"

class StTriggerSimuMaker;
class StMuDstMaker;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgSoftMuDstTriggerSimuMakerFactory : public StjTrgSoftFactory {

public:
  StjTrgSoftMuDstTriggerSimuMakerFactory(StTriggerSimuMaker* simuTrig, StMuDstMaker* uDstMaker, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap)
    : _simuTrig(simuTrig), _uDstMaker(uDstMaker), _bemcJpTowerMap(bemcJpTowerMap) { }
  virtual ~StjTrgSoftMuDstTriggerSimuMakerFactory() { }

  StjTrgSoft* create();

private:
  StTriggerSimuMaker* _simuTrig;
  StMuDstMaker* _uDstMaker;
  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  ClassDef(StjTrgSoftMuDstTriggerSimuMakerFactory, 1)

};


#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
