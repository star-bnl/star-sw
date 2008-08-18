// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareTriggerSimuMakerFactory.h,v 1.2 2008/08/18 06:20:47 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StjTrgMuDstSoftwareFactory.h"

class StTriggerSimuMaker;
class StMuDstMaker;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgMuDstSoftwareTriggerSimuMakerFactory : public StjTrgMuDstSoftwareFactory {

public:
  StjTrgMuDstSoftwareTriggerSimuMakerFactory(StTriggerSimuMaker* simuTrig, StMuDstMaker* uDstMaker, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap)
    : _simuTrig(simuTrig), _uDstMaker(uDstMaker), _bemcJpTowerMap(bemcJpTowerMap) { }
  virtual ~StjTrgMuDstSoftwareTriggerSimuMakerFactory() { }

  StjTrgMuDstSoftware* create();

private:
  StTriggerSimuMaker* _simuTrig;
  StMuDstMaker* _uDstMaker;
  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  ClassDef(StjTrgMuDstSoftwareTriggerSimuMakerFactory, 1)

};


#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
